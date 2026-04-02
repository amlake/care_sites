# module load GCC/6.4.0-2.28  CUDA/9.0.176  OpenMPI/2.1.1 Python/3.6.3 numpy/1.13.1-Python-3.6.3 scikit-learn/0.19.1-Python-3.6.3 pandas/0.18.1-Python-3.6.3

import os
import pandas as pd
import numpy as np
import sklearn as sklearn
from sklearn.decomposition import LatentDirichletAllocation

os.chdir("/data/davis_lab/allie/care_sites/output/SpecialtyPrediction")

def run_lda_analysis(dtm, care_site_info, file_suffix):
    # 1. Fit the LDA model
    num_topics = 50 
    lda_model = LatentDirichletAllocation(n_components=num_topics, random_state=42)
    lda_model.fit(dtm)

    # 2. Display topics
    terms = dtm.columns if isinstance(dtm, pd.DataFrame) else range(dtm.shape[1])
    print(f"\nTopics for {file_suffix}:")
    for topic_idx, topic in enumerate(lda_model.components_):
        print(f"Topic #{topic_idx}:")
        print(" ".join([terms[i] for i in topic.argsort()[:-6:-1]]))

    # Create topic probabilities from LDA model
    topic_probs = lda_model.transform(dtm)
    topic_cols = [f'Topic_{i}' for i in range(num_topics)]
    topic_df = pd.DataFrame(topic_probs, columns=topic_cols)
    topic_df['care_site_id'] = dtm.care_site_id  

    # Merge the dataframes
    merged_df = topic_df.merge(care_site_info, on='care_site_id', how='left')

    # Get dominant topic
    topic_cols = [col for col in merged_df.columns if col.startswith('Topic_')]
    merged_df['dominant_topic'] = merged_df[topic_cols].idxmax(axis=1)
    merged_df['dominant_topic_prob'] = merged_df[topic_cols].max(axis=1)

    # Drop topic columns
    final_df = merged_df.drop(topic_cols, axis=1)

    # Reorder columns
    cols = ['care_site_id', 'dominant_topic', 'dominant_topic_prob'] 
    other_cols = [col for col in final_df.columns if col not in cols]  
    final_df = final_df[cols + other_cols] 

    # Save with full path
    output_path = f"/data/davis_lab/allie/care_sites/output/SpecialtyPrediction/care_site_topics_{file_suffix}.csv"
    final_df.to_csv(output_path, index=False)
    print(f"Saved file to: {output_path}")
    return final_df

# Import document-term matrices
dtm_phecpt = pd.read_csv("dtm_top100codes_phecpt_bycaresite.txt", sep="\t")
dtm_phe = pd.read_csv("dtm_top100codes_phe_bycaresite.txt", sep="\t")
dtm_cpt = pd.read_csv("dtm_top100codes_cpt_bycaresite.txt", sep="\t")

# Import care site info
care_site_info = pd.read_excel("/data/davis_lab/allie/care_sites/output/CareSite_VisitDetailCount_FILTERED_122924.xlsx")

# Run analysis for each
phe_results = run_lda_analysis(dtm_phe, care_site_info, "phe_only")
cpt_results = run_lda_analysis(dtm_cpt, care_site_info, "cpt_only")
phecpt_results = run_lda_analysis(dtm_phecpt, care_site_info, "phecpt_only")