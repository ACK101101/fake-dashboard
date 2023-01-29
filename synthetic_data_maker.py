import pandas as pd
from faker import Faker
import random

ROWS = 1000
NUM_NAMES = 10

columns = ["Judge_Name",    # name
        "Judge_Reject",     # Yes / No
        "Judge_Impose",     # Yes / No    
        "Judge_Impose2",    # Less / Unclear / More / Unknown

        "Def_Race",         # 
        "Court",
        "Def_Gender",
        "AnyCriminalHistory",

        "crimetype_people",
        "crimetype_violent",
        "crimetype_property",
        "crimetype_drug",
        "crimetype_mva",
        "crimetype_financial",
        "crimetype_firearms",

        "days_btwn_arrest_plea",
        "days_btwn_plea_tender",

        "DefAttorneyType",

        "X1stCharge_Status",
        "X1stPROS_SentLength",
        "X1stPROS_ProbLength",
        "X1stPROS_SentenceType",
        "RehabTreatment",
        "X1stPROS_Prison.",
        "X1stPROS_Probation"]

data = pd.read_csv("plea_tracker_06.25.22_cleaned.csv")

# Get data info
for c in columns:
    print(c, ": ")
    print(data[c].unique())
    print("\n")

# setup pools of fake data to draw from
fake = Faker()

Judge_Reject = ['No','Yes']
Judge_Impose = ['No','Yes']
Judge_Impose2 = ['Less', 'Unclear', 'More', 'Unknown']
Judge_Name = []
for i in range(NUM_NAMES):
    Judge_Name.append(fake.name())

Def_Race = ['White', 'Unknown', 'Other', 'Black', 'Hispanic']
Court = ['Superior Court', 'District Court', 'Not yet determined']
Def_Gender = ['Female', 'Male', 'Other (please specify their preferred gender identity, if known)']
AnyCriminalHistory = [0, 1]

crimetype_people = [0, 1]
crimetype_violent = [0, 1]
crimetype_property = [0, 1]
crimetype_drug = [0, 1]
crimetype_mva = [0, 1]
crimetype_financial = [0, 1]
crimetype_people = [0, 1]
crimetype_people = [0, 1]
crimetype_firearms = [0, 1]

days_btwn_arrest_plea = [] ###
days_btwn_plea_tender = [] ###

DefAttorneyType = ['a court-appointed lawyer', 'a private attorney', 'from CPCS', 'unknown']
X1stCharge_Status = ['Guilty verdict/finding', 'Continuance without a finding', '267/87','Dismissed', 'Nolle Prosequi', 'Other']
X1stPROS_SentLength = [] ###
X1stPROS_ProbLength = [] ###
X1stPROS_SentenceType = ['Probation', 'State Prison', 'House of Corrections', 'State Prison,Probation', 'County/Split']

RehabTreatment = ['No','Yes']
X1stPROS_Prison = ['No','Yes']
X1stPROS_Probation = ['No','Yes']

# get and add fake data
df = pd.DataFrame()
for i in range(ROWS):
    df.loc[i, "Judge_Name"] = random.choice(Judge_Name)
    df.loc[i, "Judge_Reject"] = random.choice(Judge_Reject)
    df.loc[i, "Judge_Impose"] = random.choice(Judge_Impose)
    df.loc[i, "Judge_Impose2"] = random.choice(Judge_Impose2)
    
    df.loc[i, "Def_Race"] = random.choice(Def_Race)
    df.loc[i, "Court"] = random.choice(Court)
    df.loc[i, "Def_Gender"] = random.choice(Def_Gender)
    df.loc[i, "AnyCriminalHistory"] = random.choice(AnyCriminalHistory)
    
    df.loc[i, "crimetype_people"] = random.choice(crimetype_people)
    df.loc[i, "crimetype_violent"] = random.choice(crimetype_violent)
    df.loc[i, "crimetype_property"] = random.choice(crimetype_property)
    df.loc[i, "crimetype_drug"] = random.choice(crimetype_drug)
    df.loc[i, "crimetype_mva"] = random.choice(crimetype_mva)
    df.loc[i, "crimetype_financial"] = random.choice(crimetype_financial)
    df.loc[i, "crimetype_people"] = random.choice(crimetype_people)
    df.loc[i, "crimetype_firearms"] = random.choice(crimetype_firearms)
    
    df.loc[i, "days_btwn_arrest_plea"] = random.randint(100, 1000)
    df.loc[i, "days_btwn_plea_tender"] = random.randint(1, 800)
    
    df.loc[i, "DefAttorneyType"] = random.choice(DefAttorneyType)
    df.loc[i, "X1stCharge_Status"] = random.choice(X1stCharge_Status)
    df.loc[i, "X1stPROS_SentLength"] = random.randint(0, 10)
    df.loc[i, "X1stPROS_ProbLength"] = random.randint(0, 50)
    df.loc[i, "X1stPROS_SentenceType"] = random.choice(X1stPROS_SentenceType)
    
    df.loc[i, "RehabTreatment"] = random.choice(RehabTreatment)
    df.loc[i, "X1stPROS_Prison."] = random.choice(X1stPROS_Prison)
    df.loc[i, "X1stPROS_Probation"] = random.choice(X1stPROS_Probation)

df.to_csv("fakeData.csv")
    