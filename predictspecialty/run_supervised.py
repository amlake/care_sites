# module load GCC/6.4.0-2.28  CUDA/9.0.176  OpenMPI/2.1.1 Python/3.6.3 numpy/1.13.1-Python-3.6.3 scikit-learn/0.19.1-Python-3.6.3 pandas/0.18.1-Python-3.6.3

import os
import pandas as pd
import numpy as np
import sklearn as sklearn
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import LinearSVC
from sklearn.neural_network import MLPClassifier
from sklearn.model_selection import cross_val_score
from sklearn.metrics import classification_report, accuracy_score, precision_score, recall_score, f1_score

os.chdir("/data/davis_lab/allie/care_sites/output/SpecialtyPrediction")

# Import document-term matrices
dtm_phe = pd.read_csv("dtm_topcodes_phe_bycaresite.txt", sep="\t")
dtm_cpt = pd.read_csv("dtm_topcodes_cpt_bycaresite.txt", sep="\t") 
dtm_phecpt = pd.read_csv("dtm_topcodes_phecpt_bycaresite.txt", sep="\t")

# Import care site info
care_site_info = pd.read_excel("../CareSite_VisitDetailCount_FILTERED_AML_010425.xlsx")

# Create final dataframe
dtm_phecpt_specialty = dtm_phecpt.merge(care_site_info[['care_site_id', 'MappedSpecialty']], on='care_site_id', how='left')

# Assuming 'specialty' is your outcome column
# First, remove any non-numeric columns except specialty
X = dtm_phecpt_specialty.select_dtypes(include=[np.number])  # Keep only numeric columns
y = dtm_phecpt_specialty['MappedSpecialty']  # Replace 'specialty' with your actual column name

# After loading your data, add this to filter out rare specialties
MIN_SAMPLES = 10
specialty_counts = y.value_counts()
valid_specialties = specialty_counts[specialty_counts >= MIN_SAMPLES].index
mask = y.isin(valid_specialties)
mask.value_counts()

# Filter X and y to only include specialties with sufficient samples
X = X[mask]
y = y[mask]

# Encode the specialty labels
le = LabelEncoder()
y_encoded = le.fit_transform(y)

models = {
    'Logistic Regression': LogisticRegression(multi_class='ovr', solver='lbfgs', max_iter=1000),
    'Random Forest': RandomForestClassifier(n_estimators=100, random_state=42),
    'Linear SVM': LinearSVC(random_state=42, max_iter=1000),
    'Neural Network': MLPClassifier(hidden_layer_sizes=(100,50), max_iter=500, random_state=42)
}

# Create empty lists to store both types of results
performance_metrics = []  # for specialty-stratified metrics
overall_metrics = []      # for overall metrics

# Train and evaluate each model
for name, model in models.items():
    print(f"\n{name}:")
    
    # Split data
    X_train, X_test, y_train, y_test = train_test_split(X, y_encoded, test_size=0.2, random_state=42)
    y_test_labels = le.inverse_transform(y_test)
    
    # Perform 5-fold cross-validation
    cv_scores = cross_val_score(model, X, y_encoded, cv=5)
    
    # Fit model and get predictions
    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)
    y_pred_labels = le.inverse_transform(y_pred)
    
    # Get specialty-stratified metrics
    class_report = classification_report(y_test_labels, y_pred_labels)
    
    # Parse the specialty-stratified metrics
    lines = class_report.split('\n')
    for line in lines[2:-3]:  # Skip header and footer lines
        if line.strip():
            parts = line.strip().split()
            class_name = parts[0]
            metrics = {
                'precision': float(parts[1]),
                'recall': float(parts[2]),
                'f1-score': float(parts[3]),
                'support': int(parts[4])
            }
            performance_metrics.append({
                'model': name,
                'specialty': class_name,
                'precision': metrics['precision'],
                'recall': metrics['recall'],
                'f1-score': metrics['f1-score'],
                'support': metrics['support'],
                'cv_mean': cv_scores.mean(),
                'cv_std': cv_scores.std() * 2
            })
    
    # Calculate overall metrics only for valid specialties
    # First, check which specialties are present in test and predictions
    test_specialties = set(y_test_labels)
    pred_specialties = set(y_pred_labels)
    actual_valid_specialties = [s for s in valid_specialties if s in test_specialties and s in pred_specialties]
    
    print(f"\nSpecialties in test set: {len(test_specialties)}")
    print(f"Specialties in predictions: {len(pred_specialties)}")
    print(f"Valid specialties with samples in both: {len(actual_valid_specialties)}")
    
    # Calculate metrics only for specialties that appear in both test and predictions
    accuracy = accuracy_score(y_test_labels, y_pred_labels)
    precision = precision_score(y_test_labels, y_pred_labels, 
                              average='macro', 
                              labels=actual_valid_specialties)
    recall = recall_score(y_test_labels, y_pred_labels, 
                         average='macro', 
                         labels=actual_valid_specialties)
    f1 = f1_score(y_test_labels, y_pred_labels, 
                  average='macro', 
                  labels=actual_valid_specialties)
    
    overall_metrics.append({
        'model': name,
        'accuracy': accuracy,
        'precision': precision,
        'recall': recall,
        'f1-score': f1,
        'cv_mean': cv_scores.mean(),
        'cv_std': cv_scores.std() * 2
    })

# Convert both to DataFrames and save
performance_df = pd.DataFrame(performance_metrics)
overall_df = pd.DataFrame(overall_metrics)

# Save both metrics
performance_df.to_csv('model_specialty_performance.csv', index=False)
overall_df.to_csv('model_overall_performance.csv', index=False)

# Print overall results
print("\nOverall Model Performance:")
print(overall_df.round(3))
