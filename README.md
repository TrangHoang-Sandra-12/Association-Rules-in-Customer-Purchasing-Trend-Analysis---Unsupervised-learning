🛍️ Association Rules in Customer Purchasing Trend Analysis  
Author: Trang Hoang  
Language: R  
RPubs Report: https://rpubs.com/Trang_Hoang/1262505

📘 Overview

This project applies Association Rule Mining to explore purchasing patterns and behavioral trends among customers.
Using the Apriori and Classification Association Rules (CARs) approach, the analysis identifies relationships between product categories, demographics, and purchase frequency — providing insights for targeted marketing, loyalty strategy, and inventory planning.

🎯 Objectives

Identify frequent itemsets and relationships between customer attributes

Uncover behavioral patterns driving purchase frequency

Generate and visualize association rules based on support, confidence, and lift

Derive actionable insights to improve marketing segmentation

📊 Dataset

Source: Kaggle – Customer Shopping Trends Dataset

| Variable                                                | Description                                        |
| ------------------------------------------------------- | -------------------------------------------------- |
| `Age`, `Gender`                                         | Demographics                                       |
| `Item.Purchased`, `Category`, `Size`, `Color`           | Product details                                    |
| `Frequency.of.Purchases`                                | Target variable (e.g., weekly, monthly, quarterly) |
| `Location`, `Season`                                    | Contextual attributes                              |
| `Payment.Method`, `Discount.Applied`, `Promo.Code.Used` | Purchase behavior                                  |
| `Review.Rating`, `Previous.Purchases`                   | Customer feedback & loyalty                        |

⚙️ Methodology
🔹 1. Data Processing

Cleaned and converted categorical variables to factors

Discretized numeric features using supervised methods:

MDLP (Minimal Description Length Principle) — not optimal

CAIM (Class-Attribute Interdependence Maximization) — more effective for splitting continuous features

🔹 2. Mining Association Rules

Used Classification Association Rules (CARs) with target variable Frequency.of.Purchases

Applied parameters:

Support: 3%

Confidence: 10%

Filtered redundant and insignificant rules using:

is.redundant(), is.significant(), and is.maximal()

🔹 3. Visualization

Interactive rule exploration using inspectDT()

Network visualization of rules (arulesViz package)

Scatter plot of support vs. lift, shaded by confidence

📈 Key Results

Generated ~120 strong rules, revealing diverse customer purchase behaviors.

High-frequency shoppers (weekly/monthly):

Age 22–30, typically size M, unaffected by discounts or subscriptions.

Moderate-frequency shoppers (every 3 months):

Predominantly male, prefer cash payments, size L, high review scores (>4/5).

Low-frequency shoppers (quarterly/yearly):

Often former frequent buyers with poor product experiences → churn risk.

Metric	Observation
Support	≥ 0.03
Confidence	≥ 0.10
Number of Rules	~120
Top Predictors	Age, Size, Payment Method, Review Rating

💡 Insights & Recommendations

Targeted Marketing: Focus retention campaigns on customers showing reduced frequency but high past engagement.

Product Strategy: Promote mid-size (M–L) products to younger segments (22–30).

Loyalty & Rewards: Offer quality assurance or after-sale benefits to rebuild trust among churned users.

Payment Optimization: Incentivize digital payment adoption among male, cash-preferred shoppers.

🧰 Tools & Libraries

Language: R

Libraries: arules, arulesViz, arulesCBA

Techniques:

Data discretization (CAIM)

Association Rule Mining (Apriori, CARs)

Network visualization and lift–support mapping

Environment: RStudio


👩‍💻 Author

Trang Hoang  
🎓 Master’s Student in Data Science & Business Analytics, University of Warsaw  
💼 Formerly @ MoMo E-Wallet | Shopee Vietnam | NielsenIQ  
📫 trangphuong83p@gmail.com
  
🌐 RPubs Portfolio: https://rpubs.com/Trang_Hoang
