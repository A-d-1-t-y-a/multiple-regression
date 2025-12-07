# ============================================================================
# Data Analytics Coursework - Demo Presentation Script
# Task 3: Multivariate Data Analysis - GROUP DEMO
# ============================================================================
# Duration: 15 minutes
# Group Members: Person 1, Person 2, Person 3
# ============================================================================

## INTRODUCTION (1 minute) - Person 1

**Person 1:**
"Good [morning/afternoon]. We are presenting our analysis of the IMD 2025 dataset,
examining multiple dimensions of deprivation across English Local Authority Districts.

Our group consists of three members:
- Person 1: Handled data augmentation and correlation analysis
- Person 2: Conducted PCA and North-South classification
- Person 3: Performed cluster analysis and spatial visualization

Let me begin with Part 1..."

---

## PART 1: DATA AUGMENTATION & CORRELATION (3 minutes) - Person 1

**Person 1:**
"First, we needed to augment missing Region values in our dataset.

[SHOW CODE: Lines 30-50 of task3_complete.R]

We created a County-to-Region lookup by joining two geographic datasets:
- Local Authority District to County
- County to Region

This allowed us to fill in [X] missing Region values.

[SHOW OUTPUT: Region summary table]

Next, we examined correlations between the 7 IMD domains using ggpairs().

[SHOW: Scatter matrix plot]

**Key Findings:**
1. **Strong correlations (r > 0.80):**
   - Income and Employment are highly correlated
   - These represent economic deprivation

2. **Moderate correlations (0.65-0.80):**
   - Education correlates with Health and Income
   - Forms a social deprivation cluster

3. **Weak correlations:**
   - Barriers to Housing shows weakest correlations
   - Suggests it captures a distinct deprivation dimension

This correlation structure will inform our PCA analysis."

---

## PART 2: NORTH VS SOUTH CLASSIFICATION (2 minutes) - Person 2

**Person 2:**
"Moving to Part 2, we investigated whether two variables could predict
North vs South location.

[SHOW CODE: Lines 95-130]

We defined:
- North: North East, North West, Yorkshire & Humber
- South: South East, South West, East of England

[SHOW OUTPUT: Domain comparison table]

**Recommended variables:** [Variable 1] and [Variable 2]

**Justification:**
These show the largest mean differences between North and South regions.

[SHOW: Scatter plot with ellipses]

**Difficult-to-predict districts:**
[SHOW OUTPUT: Top 10 ambiguous districts]

These districts have similar distances to both North and South centroids,
indicating intermediate deprivation patterns that don't fit the regional stereotype."

---

## PART 3: PRINCIPAL COMPONENT ANALYSIS (4 minutes) - Person 2

**Person 2:**
"For Part 3, we performed PCA on the 7 IMD domains.

[SHOW: Screeplot]

**Variance explained:**
- PC1: [X]% - represents overall deprivation
- PC2: [Y]% - contrasts economic vs environmental factors
- PC3: [Z]% - captures residual variation

[SHOW: PC1 vs PC2 Biplot]

**PC1 Interpretation:**
All domains load positively and similarly on PC1.
This is a general deprivation factor - high PC1 means high deprivation overall.

**PC2 Interpretation:**
PC2 contrasts domains:
- Positive: Economic deprivation (Income, Employment)
- Negative: Environmental/housing issues (Barriers, Living)

[SHOW: PC2 vs PC3 Biplot]

PC3 provides additional nuance, possibly related to Crime patterns.

[SHOW: Region effects in biplots]

Different regions cluster differently, showing distinct deprivation profiles.

**London Comparison:**

[SHOW CODE: PCA on London only]

When we analyze London separately:
- PC1 still represents overall deprivation
- But variance explained is [higher/lower]: [X]% vs [Y]%
- Loading patterns differ slightly

This confirms London has a unique deprivation structure compared to England overall."

---

## PART 4: CLUSTER ANALYSIS (3 minutes) - Person 3

**Person 3:**
"For Part 4, we performed hierarchical clustering on both districts and domains.

**Part 4(a): Clustering Districts**

[SHOW OUTPUT: Comparison table of methods]

We compared:
- Distance metrics: Euclidean, Manhattan
- Linkage methods: Single, Complete, Average, Ward

**Best method:** [Method] with [Distance] distance
**Agglomerative coefficient:** [X]

[SHOW: Dendrogram for districts]

**Interpretation:**
- The dendrogram shows [3-5] main clusters
- These represent different types/levels of deprivation
- Height indicates dissimilarity between clusters

**Part 4(b): Clustering Domains**

[SHOW: Dendrogram for domains]

**Interpretation:**
The domain clustering reveals:
1. Income + Employment cluster (economic deprivation)
2. Education + Health cluster (social deprivation)
3. Barriers + Living cluster (environmental)
4. Crime may cluster separately

This confirms our correlation analysis from Part 1."

---

## PART 5: CHOROPLETH MAPS (1.5 minutes) - Person 3

**Person 3:**
"Finally, Part 5 examines spatial distribution.

[NOTE: If shapefiles available, SHOW maps. Otherwise, discuss expected patterns]

**Expected observations:**

1. **Overall Deprivation Map:**
   - Geographic clustering of high/low deprivation
   - Urban centers show distinct patterns
   - Coastal areas may differ from inland

2. **PC1 Map:**
   - Should closely resemble Overall map
   - PC1 = general deprivation factor

3. **PC2 Map:**
   - Highlights regional differences in deprivation TYPE
   - North-South divide may be visible
   - Economic vs environmental deprivation patterns

**Spatial patterns:**
- London shows unique characteristics
- Northern cities cluster together
- Rural/coastal areas form distinct groups"

---

## CONCLUSION (0.5 minutes) - Person 1

**Person 1:**
"To summarize:
- We successfully augmented the dataset and identified correlation patterns
- PCA revealed 3 meaningful components explaining [X]% of variance
- Cluster analysis confirmed domain groupings and district types
- Spatial analysis would reveal geographic deprivation patterns

Our analysis demonstrates how multivariate methods reveal complex
deprivation structures that single variables cannot capture.

We're happy to answer any questions."

---

## ANTICIPATED QUESTIONS & ANSWERS

**Q1: Why did you choose those specific methods for clustering?**
**A (Person 3):** "We compared multiple methods using the agglomerative coefficient.
Ward's method typically performs well for creating compact, interpretable clusters,
while the choice of distance metric depends on whether we want to emphasize
absolute differences (Euclidean) or component-wise differences (Manhattan)."

**Q2: How do you interpret negative loadings in PCA?**
**A (Person 2):** "Negative loadings indicate inverse relationships. For example,
if PC2 has positive loadings for Income/Employment and negative for Barriers/Living,
it means PC2 contrasts economic vs environmental deprivation. High PC2 = more
economic deprivation relative to environmental issues."

**Q3: Why is London different from other regions?**
**A (Person 1/2):** "London has unique socio-economic characteristics: higher
income inequality, different housing markets, and distinct crime patterns.
This creates a different deprivation structure, which our PCA comparison revealed."

**Q4: How many clusters should you choose for the districts?**
**A (Person 3):** "The dendrogram suggests 3-5 main clusters. The optimal number
depends on the application. For policy purposes, 3-4 clusters might be most
interpretable, representing high/medium/low deprivation with possible subtypes."

**Q5: What are the limitations of your analysis?**
**A (All):** "Main limitations include:
- Synthetic data (if using our generated data)
- Missing spatial data for full choropleth analysis
- PCA assumes linear relationships
- Hierarchical clustering is sensitive to outliers
- Regional definitions are somewhat arbitrary"

---

## TECHNICAL SETUP CHECKLIST

Before the demo:
- [ ] R and RStudio installed and working
- [ ] All required packages loaded
- [ ] Data files in correct directory
- [ ] Script runs without errors
- [ ] Plots display correctly
- [ ] Each person knows their sections
- [ ] Timing practiced (15 minutes total)
- [ ] Backup plan if code fails (screenshots of outputs)

---

## TIMING BREAKDOWN

- Introduction: 1 minute
- Part 1 (Person 1): 3 minutes
- Part 2 (Person 2): 2 minutes
- Part 3 (Person 2): 4 minutes
- Part 4 (Person 3): 3 minutes
- Part 5 (Person 3): 1.5 minutes
- Conclusion: 0.5 minutes
- **Total: 15 minutes**
- Buffer for questions: 5-10 minutes

---

## PRESENTATION TIPS

1. **Practice transitions** between speakers
2. **Have code ready** - don't type during demo
3. **Explain outputs** - don't just show them
4. **Make eye contact** with assessors
5. **Speak clearly** and at moderate pace
6. **Point to specific** numbers/patterns in outputs
7. **If code fails**, explain what should happen
8. **Stay within time** - practice beforehand

---

## CODE SECTIONS TO DEMONSTRATE

**Person 1:**
- Lines 30-60: Data augmentation
- Lines 70-90: ggpairs() and correlation matrix

**Person 2:**
- Lines 95-140: North vs South analysis
- Lines 160-240: PCA analysis and interpretation
- Lines 250-280: London PCA comparison

**Person 3:**
- Lines 300-350: District clustering
- Lines 360-400: Domain clustering
- Lines 420-450: Choropleth map code/discussion

Good luck with your presentation!
