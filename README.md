#### 💭 In this project I use R to perform PCA on Nile Tilapia Retinal Gene Expression Data.
Description:  The Nile tilapia (Oreochromis niloticus; Fig. 1) serves as an excellent model organism for studying the genetic basis of phototaxis, the movement of an organism in response to light stimuli. Phototaxis is crucial in various behaviors such as foraging, predator avoidance, and reproductive success. Understanding the molecular mechanisms underlying phototaxis in Nile tilapia can provide valuable insights into the evolution and adaptation of visual systems in aquatic organisms. In the Nile tilapia, there is a developmental shift. Juvenile fish exhibit increased kinesis, or random movement or activity without a specific direction or orientation, in response to light (Fig. 2A). However, this behavior changes with age. By 17dph (days post fertilization) this behavior switches such that increased kinesis occurs in darkness (Fig. 2B). Because kinesis can be an indicator of stress, the behavioral switch suggests a life history change from hiding to seeking. The retina, a specialized tissue at the back of the eye, plays a central role in photokinesis by converting light stimuli into neural signals. Changes in gene expression within the retina are likely to be associated with differences in phototactic behavior during development. We used 3’Tagseq to sequence RNA from the retina’s of 12dph and 17dph individuals. 
#### The steps I took to complete this experiment was 
1. Importing necessary **libraries**
2. Importing counts and individual data
3. **Cleaning** data
4. Performing **TPM and log transformations**
5. **PCA Analysis** and dropping **outliers**
6. Graping PCAs using **Scatter and Scree Plots**

#### **Results...**
- After removing outliers in the data, the first 2 PCAs accounted for ~20% of the total variation in the dataset
- Although this is not as much variation as I had wished to see, this still provides important information about where variation lies in the dataset, for example, the first PCA that was selected was the Day of the tilapia's birth, and the second PCA feature was the mother.
- Following this PCA expirement, it will help to perform statistical tests on the first features to determine their significance. For example, ANOVA tests and Deseq are great potential next steps to take.
