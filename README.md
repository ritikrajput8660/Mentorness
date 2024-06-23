# Mentorness


The dataset contains the following columns:

Transaction_ID: Unique identifier for each transaction.
Timestamp: Date and time of the transaction.
Vehicle_Type: Type of vehicle involved in the transaction.
FastagID: Unique identifier for Fastag.
TollBoothID: Identifier for the toll booth.
Lane_Type: Type of lane used for the transaction.
Vehicle_Dimensions: Dimensions of the vehicle.
Transaction_Amount: Amount associated with the transaction.
Amount_paid: Amount paid for the transaction.
Geographical_Location: Location details of the transaction.
Vehicle_Speed: Speed of the vehicle during the transaction.
Vehicle_Plate_Number: License plate number of the vehicle.
Fraud_indicator: Binary indicator of fraudulent activity (target variable).
Project Overview
This project aims to develop a fraud detection system for toll transactions using machine learning techniques. The dataset provided contains information about various toll transactions, including details such as transaction ID, timestamp, vehicle type, transaction amount, and a binary indicator of fraudulent activity.

Approach
Data Preprocessing: The dataset will be preprocessed to handle missing values, encode categorical variables, and scale numerical features if necessary.
Model Training: Several machine learning models will be trained on the preprocessed data, including decision trees, random forests, and gradient boosting classifiers.
Model Evaluation: The performance of each model will be evaluated using appropriate metrics such as accuracy, precision, recall, and F1-score.
Model Selection: The best-performing model will be selected based on evaluation metrics and deployed for fraud detection.
Usage
To use this fraud detection system:

Data Preparation: Ensure the input data is in the same format as the provided dataset, with the required columns.
Model Deployment: Deploy the trained machine learning model to predict fraudulent transactions based on the input data.
Prediction: Use the deployed model to predict whether a transaction is fraudulent or not based on its features.
Requirements
To run the code, the following dependencies are required:

Python 3.x
Jupyter Notebook
scikit-learn
pandas
gradio
Contributors
