"""
Python script to combine and process multiple IMDB dataset files into a single CSV
"""

import pandas as pd
import numpy as np
import os
import sys

if all(
    [
        os.path.exists(f)
        for f in [
            "name.basics.tsv",
            "title.akas.tsv",
            "title.basics.tsv",
            "title.principals.tsv",
            "title.ratings.tsv",
        ]
    ]
):
    print(
        "Pre-requisite files exist in the current directory. Proceeding with data preprocessing..."
    )
else:
    print(
        """Pre-requisite files are missing in the current directory. 
        Please download from https://www.imdb.com/interfaces/ and 
        extract the datasets with the appropriate names"""
    )
    sys.exit()

# read the files
name_basics = pd.read_csv("name.basics.tsv", sep="\t")
akas = pd.read_csv("title.akas.tsv", sep="\t")
title_basics = pd.read_csv("title.basics.tsv", sep="\t")
principals = pd.read_csv("title.principals.tsv", sep="\t")
ratings = pd.read_csv("title.ratings.tsv", sep="\t")

# select only relevant columns from name_basics
name_basics = name_basics.loc[:, "nconst":"primaryName"]

# select only relevant columns from akas and rename 'titleId' to 'tconst'
akas = akas[["titleId", "title", "region", "language", "isOriginalTitle"]]
akas = akas.query("isOriginalTitle == 0")
akas = akas.rename(columns={"titleId": "tconst"})

# select only relevant columns from title_basics
title_basics = title_basics[
    ["tconst", "titleType", "primaryTitle", "startYear", "runtimeMinutes", "genres"]
]


# select only actors and actresses from principals and only relevant columns
principals = principals.query('category in ("actor", "actress", "self")')
principals = principals[["tconst", "nconst"]]

# select only relevant columns from ratings
ratings = ratings[["tconst", "averageRating", "numVotes"]]

# merge dataframes
imdb = pd.merge(title_basics, ratings, how="left", on="tconst")
imdb = pd.merge(imdb, akas, how="outer", on="tconst")
imdb = pd.merge(imdb, principals, how="outer", on="tconst")
imdb = pd.merge(imdb, name_basics, how="left", on="nconst")

# select only movies
imdb = imdb.query('titleType == "movie"')

# replace '\N' with NaN and drop missing values
imdb = imdb.replace(r"\\N", np.NaN, regex=True)
imdb = imdb.dropna()

# drop unneeded columns
imdb = imdb.drop(columns=["titleType", "title", "isOriginalTitle", "nconst"])

# change year and runtime to int data type, genres to string
imdb["startYear"] = imdb["startYear"].astype("Int64")
imdb["runtimeMinutes"] = imdb["runtimeMinutes"].astype("Int64")
imdb["genres"] = imdb["genres"].astype("string")

# explode genres
imdb = imdb.drop("genres", axis=1).join(
    imdb["genres"]
    .str.split(",", expand=True)
    .stack()
    .reset_index(level=1, drop=True)
    .rename("genres")
)

# select movies from 2011 to 2022 in the genres of interest
imdb = imdb.query("2010 < startYear < 2023")
imdb = imdb.query(
    'genres in ["Action", "Crime", "Horror", "Mystery", "Romance", "Sci-Fi", "Thriller"]'
)

# print the number of unique movie titles
print(f"Extracted {imdb['primaryTitle'].nunique()} unique movie titles")

data_path = r"../data/"
file_name = "imdb_viz.csv"
full_path = os.path.join(data_path, file_name)

# save to CSV
imdb.to_csv(full_path, index=False)
