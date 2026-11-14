# Mathematical Specification & Invariants (`Obook`)

This document specifies the exact mathematical identities and exchange invariants enforced by `Obook`.

---

## 1. 128-Bit Intermediate Fixed-Point Arithmetic (`Obook.Fixed`)

All prices and quantities are represented as signed 64-bit integers (`int64`) scaled by $S = 10^8$:
$$X_{\text{real}} = \frac{X_{\text{raw}}}{10^8}$$

### 1.1 Exact Multiplication Without `int64` Overflow
Given unsigned magnitudes $u_a, u_b \in [0, 2^{63}-1]$, we decompose both operands by Euclidean division by $S = 10^8$:
$$u_a = q_a S + r_a, \quad u_b = q_b S + r_b \quad (0 \le r_a, r_b < 10^8)$$
Then:
$$\frac{u_a \times u_b}{S} = (q_a q_b) S + q_a r_b + q_b r_a + \frac{r_a r_b}{S}$$
Because $r_a r_b < 10^{16} < 2^{54} \ll 2^{63}-1$, the low product $r_a r_b$ never overflows 64-bit signed arithmetic, yielding both the exact quotient and exact remainder $(r_a r_b) \bmod S$ for deterministic rounding.

### 1.2 Exact Division via 27-Bit Remainder Shift-and-Add
Given $u_a = q_0 u_b + r_0$ with $0 \le r_0 < u_b$, the scaled ratio is:
$$\frac{u_a \times S}{u_b} = q_0 S + \frac{r_0 \times S}{u_b}$$
To compute $\lfloor (r_0 \times S) / u_b \rfloor$ and $(r_0 \times S) \bmod u_b$ without $r_0 \times S$ overflowing `int64` when $u_b > 10^{11}$, `Obook.Fixed.div` runs a 27-step binary shift-and-add loop over the bits of $S = 10^8 < 2^{27}$, testing `rem >= ub - rem` and `rem >= ub - r0` at each step so no intermediate value ever exceeds $u_b \le 2^{63}-1$.

---

## 2. Conservation of Volume & Book Audit Invariants (`Obook.Book`)

1. **Order Accounting Identity (`Book.verify_report_conservation`):**
   Every execution report satisfies:
   $$Q_{\text{requested}} = Q_{\text{filled}} + Q_{\text{resting}} + Q_{\text{cancelled}} \quad \land \quad Q_{\text{filled}} = \sum_{f \in \text{fills}} q_f$$
2. **Uncrossed Book Invariant (`Book.audit`):**
   At the end of every matching cycle:
   $$\text{BestBid} < \text{BestAsk} \quad \lor \quad \text{Bids} = \emptyset \quad \lor \quad \text{Asks} = \emptyset$$
3. **Level Queue Sum Invariant:**
   For every active price level $L$:
   $$\text{disp\_vol}(L) = \sum_{o \in L} \text{disp\_qty}(o), \quad \text{hid\_vol}(L) = \sum_{o \in L} \text{hid\_qty}(o), \quad \text{count}(L) = |L| > 0$$

---

## 3. Streaming Microstructure Estimators (`Obook.Stats`)

### 3.1 Welford's Recurrence & Chan's Parallel Merge
For sample $x_n$:
$$\delta = x_n - \mu_{n-1}, \quad \mu_n = \mu_{n-1} + \frac{\delta}{n}, \quad M_{2,n} = M_{2,n-1} + \delta(x_n - \mu_n), \quad s^2_n = \frac{M_{2,n}}{n - 1}$$
Two independent partitions $(n_A, \mu_A, M_{2,A})$ and $(n_B, \mu_B, M_{2,B})$ merge in $O(1)$ via:
$$M_{2,AB} = M_{2,A} + M_{2,B} + (\mu_B - \mu_A)^2 \frac{n_A n_B}{n_A + n_B}$$

### 3.2 Finch's Bias-Free EWMA Variance
Seeded with $\mu_1 = x_1, \sigma^2_1 = 0$, and updated for $t \ge 2$ with decay $\alpha \in (0, 1)$:
$$\delta_t = x_t - \mu_{t-1}, \quad \mu_t = \mu_{t-1} + \alpha \delta_t, \quad \sigma^2_t = (1 - \alpha)(\sigma^2_{t-1} + \alpha \delta_t^2)$$

### 3.3 Cont, Kukanov & Stoikov (2014) Order Flow Imbalance (OFI)
$$e_t = \mathbb{I}_{\{P^b_t \ge P^b_{t-1}\}} q^b_t - \mathbb{I}_{\{P^b_t \le P^b_{t-1}\}} q^b_{t-1} - \mathbb{I}_{\{P^a_t \le P^a_{t-1}\}} q^a_t + \mathbb{I}_{\{P^a_t \ge P^a_{t-1}\}} q^a_{t-1}$$

### 3.4 Multi-Bucket VPIN (Volume-Synchronized Probability of Informed Trading)
Incoming trade volume $Q$ is sliced iteratively by $\text{take} = \min(Q_{\text{rem}}, V - v_{\text{curr}})$ across volume buckets of size $V$, maintaining a rolling ring buffer of $N$ buckets:
$$\text{VPIN} = \frac{\sum_{\tau=1}^{N} |V_\tau^{\text{buy}} - V_\tau^{\text{sell}}|}{N \cdot V} \in [0, 1]$$
