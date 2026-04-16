# Abstract 
This paper proposes a hybrid economic scenario generator designed to incorporate physical climate risk into prudential risk measurement under Solvency II, focusing on extreme losses that are central to the determination of capital requirements.

Using a dataset combining macro-financial variables and climate indicators, we document a marked empirical asymmetry. Climate conditions do not significantly affect the central dynamics of economic factors, but they have a strong and statistically significant impact on the severity of extreme losses. This result is critical from an actuarial perspective, as solvency metrics such as Value-at-Risk and Expected Shortfall are driven by the tail of the loss distribution.

Motivated by this evidence, we propose a hybrid modeling framework that separates the baseline dynamics from the treatment of extremes. The central dynamics of macro-financial variables are estimated using a nonparametric model and simulated through a block bootstrap procedure, ensuring consistency with observed temporal dependence structures. A stylized loss function is then applied to the simulated trajectories to obtain a baseline loss distribution.

Climate risk is incorporated through a peaks-over-threshold approach in which excess losses are modeled conditionally on climate regimes derived from a composite indicator capturing major physical risks relevant for insurers. We show that excess losses are significantly higher under stress climate conditions. Building on this result, we introduce a tail adjustment mechanism that modifies only the extreme part of the simulated loss distribution by replacing excesses with draws from regime-dependent empirical distributions.

This approach preserves the baseline dynamics and the frequency of exceedances while explicitly incorporating climate information into the severity of extreme losses. The results show a substantial amplification of tail risk under the climate-adjusted specification.

The proposed framework provides a tractable and empirically grounded method to integrate climate risk into economic scenario generation, with direct implications for ORSA exercises and, more broadly, for the assessment of solvency risk in insurance.


# Keywords : Climate Risk, Tail Risk, Economic Scenario Generation, Extreme Value Theory,Conditional Tail Modeling, ORSA, Capital Requirements
