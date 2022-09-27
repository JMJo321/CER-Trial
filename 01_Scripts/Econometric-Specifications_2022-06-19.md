## Econometric Specifications for CER Trials

### 1. Model for Half-Hourly Averate Treatment Effects (ATEs)

#### 1. Model with the Month-of-Year FEs

$$
\begin{align*}
kWh_{itw} \ 
& = \ \beta_{w} \mathbb{1}[\text{Treatment \& Post}]_{it} \ + \ \alpha_{iw} \ + \ \gamma_{dw} \ + \ \delta_{m} \ + \ \epsilon_{itw}
\end{align*}
$$

#### 2. Model with the Month-of-Sample (i.e., Year-by-Month) FEs

$$
\begin{align*}
kWh_{itw} \ 
& = \ \beta_{w} \mathbb{1}[\text{Treatment \& Post}]_{it} \ + \ \alpha_{iw} \ + \ \gamma_{dw} \ + \ \delta_{ym} \ + \ \epsilon_{itw}
\end{align*}
$$



### 2. Model for By-Tariff-Group Hourly ATEs in the Peak Rate Period

$$
\begin{align*}
kWh_{ith} \ 
& = \ \beta_{p} \mathbb{1}[\text{Treatment \& Post}]_{it} \ + \ \alpha_{iw} \ + \ \gamma_{dw} \ + \ \delta_{m} \ + \ \epsilon_{ith}
\end{align*}
$$



### 3. DID-Style Models for Breaking Down Hourly ATEs in the Peak Rate Period

#### 1. OLS Models

1. Base Models

   1. Model without ID-associated FEs
      $$
      \begin{align*}
      kWh_{ith} \ 
      & = \ \beta_{1} HDD_{t} \\
      & \hspace{0.7cm} + \ \beta_{2} \mathbb{1}[\text{Treatment}]_{i} \ + \ \beta_{3} HDD_{t} \mathbb{1}[\text{Treatment}]_{i} \\
      & \hspace{0.7cm} + \ \beta_{4} \mathbb{1}[\text{Post}]_{t} \ + \ \beta_{5} HDD_{t} \mathbb{1}[\text{Post}]_{t} \\
      & \hspace{0.7cm} + \ \beta_{6} \mathbb{1}[\text{Treatment \& Post}]_{it} \ + \ \beta_{7} HDD_{t} \mathbb{1}[\text{Treatment \& Post}]_{it} \\
      & \hspace{0.7cm} + \ \alpha_{dw} \ + \ \epsilon_{ith}
      \end{align*}
      $$

   2. Models with ID-associated FEs

      1. Model without Post-Indicator-Variable-related Terms
         $$
         \begin{align*}
         kWh_{ith} \ 
         & = \ \beta_{1} HDD_{t} \\
         & \hspace{0.7cm} + \ \beta_{2} HDD_{t} \mathbb{1}[\text{Treatment}]_{i} \\
         & \hspace{0.7cm} + \ \beta_{3} \mathbb{1}[\text{Treatment \& Post}]_{it} \ + \ \beta_{4} HDD_{t} \mathbb{1}[\text{Treatment \& Post}]_{it} \\
         & \hspace{0.7cm} + \ \alpha_{iw} \ + \ \gamma_{dw} \ + \ \epsilon_{ith}
         \end{align*}
         $$
         
      2. Model with Post-Indicator-Variable-related Terms
         $$
         \begin{align*}
         kWh_{ith} \ 
         & = \ \beta_{1} HDD_{t} \\
         & \hspace{0.7cm} + \ \beta_{2} HDD_{t} \mathbb{1}[\text{Treatment}]_{i} \\
         & \hspace{0.7cm} + \ \beta_{3} \mathbb{1}[\text{Post}]_{t} \ + \ \beta_{4} HDD_{t} \mathbb{1}[\text{Post}]_{t} \\
         & \hspace{0.7cm} + \ \beta_{5} \mathbb{1}[\text{Treatment \& Post}]_{it} \ + \ \beta_{6} HDD_{t} \mathbb{1}[\text{Treatment \& Post}]_{it} \\
         & \hspace{0.7cm} + \ \alpha_{iw} \ + \ \gamma_{dw} \ + \ \epsilon_{ith}
         \end{align*}
         $$
         
   
2. Model as a Function of Rate Changes

   1. Model without ID-associated FEs
      $$
      \begin{align*}
      kWh_{ith} \ 
      & = \ \beta_{1} HDD_{t} \\
      & \hspace{0.7cm} + \ \beta_{2} \mathbb{1}[\text{Treatment}]_{i} \ + \ \beta_{3} \mathbb{1}[\text{Treatment}]_{i} \Delta RC_{ip} \\
      & \hspace{0.7cm} + \ \beta_{4} HDD_{t} \mathbb{1}[\text{Treatment}]_{i} \ + \ \beta_{5} HDD_{t} \mathbb{1}[\text{Treatment}]_{i} \Delta RC_{ip} \\
      & \hspace{0.7cm} + \ \beta_{6} \mathbb{1}[\text{Post}]_{t} \ + \ \beta_{7} HDD_{t} \mathbb{1}[\text{Post}]_{t} \\
      & \hspace{0.7cm} + \ \beta_{8} \mathbb{1}[\text{Treatment \& Post}]_{it} \ + \ \beta_{9} \mathbb{1}[\text{Treatment \& Post}]_{it} \Delta RC_{ip} \\
      & \hspace{0.7cm} + \ \beta_{10} HDD_{t} \mathbb{1}[\text{Treatment \& Post}]_{it} \ + \ \beta_{11} HDD_{t} \mathbb{1}[\text{Treatment \& Post}]_{it} \Delta RC_{ip} \\
      & \hspace{0.7cm} + \ \alpha_{dw} \ + \ \epsilon_{ith}
      \end{align*}
      $$
      
   2. Model with ID-associated FEs
      $$
      \begin{align*}
      kWh_{ith} \ 
      & = \ \beta_{1} HDD_{t} \\
      & \hspace{0.7cm} + \ \beta_{2} HDD_{t} \mathbb{1}[\text{Treatment}]_{i} \ + \ \beta_{3} HDD_{t} \mathbb{1}[\text{Treatment}]_{i} \Delta RC_{ip} \\
      & \hspace{0.7cm} + \ \beta_{4} \mathbb{1}[\text{Post}]_{t} \ + \ \beta_{5} HDD_{t} \mathbb{1}[\text{Post}]_{t} \\
      & \hspace{0.7cm} + \ \beta_{6} \mathbb{1}[\text{Treatment \& Post}]_{it} \ + \ \beta_{7} \mathbb{1}[\text{Treatment \& Post}]_{it} \Delta RC_{ip} \\
      & \hspace{0.7cm} + \ \beta_{8} HDD_{t} \mathbb{1}[\text{Treatment \& Post}]_{it} \ + \ \beta_{9} HDD_{t} \mathbb{1}[\text{Treatment \& Post}]_{it} \Delta RC_{ip} \\
      & \hspace{0.7cm} + \ \alpha_{iw} \ + \ \gamma_{dw} \ + \ \epsilon_{ith}
      \end{align*}
      $$
      



#### 2. Spline Regression Models

1. Base Model
   $$
   \begin{align*}
   kWh_{ith} \ 
   & = \ \beta_{1} HDD_{t} \ + \ \beta_{2} HDD_{t}^{*} \\
   & \hspace{0.7cm} + \ \beta_{3} \mathbb{1}[\text{Treatment}]_{i} \\
   & \hspace{0.7cm} + \ \beta_{4} HDD_{t} \mathbb{1}[\text{Treatment}]_{i} \ + \ \beta_{5} HDD_{t}^{*} \mathbb{1}[\text{Treatment}]_{i} \\
   & \hspace{0.7cm} + \ \beta_{6} \mathbb{1}[\text{Post}]_{t} \ + \ \beta_{7} HDD_{t} \mathbb{1}[\text{Post}]_{t} \ + \ \beta_{8} HDD_{t}^{*} \mathbb{1}[\text{Post}]_{t} \\
   & \hspace{0.7cm} + \ \beta_{9} \mathbb{1}[\text{Treatment \& Post}]_{it} \\
   & \hspace{0.7cm} + \ \beta_{10} HDD_{t} \mathbb{1}[\text{Treatment \& Post}]_{it} \ + \ \beta_{11} HDD_{t}^{*} \mathbb{1}[\text{Treatment \& Post}]_{it} \\
   & \hspace{0.7cm} + \ \alpha_{dw} \ + \ \epsilon_{ith}
   \end{align*}
   $$

2. Model as a Function of Rate Changes
   $$
   \begin{align*}
   kWh_{ith} \ 
   & = \ \beta_{1} HDD_{t} \ + \ \beta_{2} HDD_{t}^{*} \\
   & \hspace{0.7cm} + \ \beta_{3} \mathbb{1}[\text{Treatment}]_{i} \ + \ \beta_{4} \mathbb{1}[\text{Treatment}]_{i} \Delta RC_{ip} \\
   & \hspace{0.7cm} + \ \beta_{5} HDD_{t} \mathbb{1}[\text{Treatment}]_{i} \ + \ \beta_{6} HDD_{t} \mathbb{1}[\text{Treatment}]_{i} \Delta RC_{ip} \\
   & \hspace{0.7cm} + \ \beta_{7} HDD_{t}^{*} \mathbb{1}[\text{Treatment}]_{i} \ + \ \beta_{8} HDD_{t}^{*} \mathbb{1}[\text{Treatment}]_{i} \Delta RC_{ip} \\
   & \hspace{0.7cm} + \ \beta_{9} \mathbb{1}[\text{Post}]_{t} \ + \ \beta_{10} HDD_{t} \mathbb{1}[\text{Post}]_{t} \ + \ \beta_{11} HDD_{t}^{*} \mathbb{1}[\text{Post}]_{t} \\
   & \hspace{0.7cm} + \ \beta_{12} \mathbb{1}[\text{Treatment \& Post}]_{it} \ + \ \beta_{13} \mathbb{1}[\text{Treatment \& Post}]_{it} \Delta RC_{ip} \\
   & \hspace{0.7cm} + \ \beta_{14} HDD_{t} \mathbb{1}[\text{Treatment \& Post}]_{it} \ + \ \beta_{15} HDD_{t} \mathbb{1}[\text{Treatment \& Post}]_{it} \Delta RC_{ip} \\
   & \hspace{0.7cm} + \ \beta_{16} HDD_{t}^{*} \mathbb{1}[\text{Treatment \& Post}]_{it} \ + \ \beta_{17} HDD_{t}^{*} \mathbb{1}[\text{Treatment \& Post}]_{it} \Delta RC_{ip} \\
   & \hspace{0.7cm} + \ \alpha_{dw} \ + \ \epsilon_{ith}
   \end{align*}
   $$



In the models above, the meaning of each subscription is as follows:

- $i$ :  Household
- $t$ :  Day
- $w$ :  30-Mintue Interval, $w \in \{ 1, 2, \cdots, 48 \}$
- $d$ :  Day of Week
- $m$ :  Month of Year
- $p$ :  Rate Period, $p \in \{ \text{Night, Day: Pre-Peak, Peak, Day: Post-Peak} \}$
- $\tau$ :  Tariff Allocation, $\tau \in \{A, B, C, D, E\}$ where $E$ means the Rate of the Control Group (i.e., Flat Rate)
- $\Delta RC_{ip}$ :  Difference from the Rate of the Control Group, i.e. $(\text{Rate})_{p \tau} \ - \ (\text{Flat Rate})$
- $HDD^{*}$:  $(HDD - Knot) \times \mathbb{1}[HDD > Knot]$

