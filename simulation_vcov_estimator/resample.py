#!/bin/env python
# -*- coding:utf-8 -*-

import argparse

import pandas as pd
import numpy as np

seed = 12345

dependent_vars = ["life_evaluation"]
noncat_fixed_vars = ["age", "security", "gdp"]
cat_fixed_vars = [
    "year",
    "gender",
    "relationship_status",
    "food",
    "housing",
    "health",
    "water",
    "air",
    "healthcare",
    "social_support",
    "respect",
    "education",
    "interesting_activity",
    "recreation",
    "occupation",
    "freedom",
]
cat_rnd_vars = ["country_name", "country_year"]


def load_data(
    filename, dependent_vars, noncat_fixed_vars, cat_fixed_vars, cat_rnd_vars
):
    df = pd.read_csv(filename, na_values=["NA"], low_memory=False)
    df.dropna(
        subset=dependent_vars + noncat_fixed_vars + cat_fixed_vars + cat_rnd_vars,
        inplace=True,
    )
    index = df.index
    df.reset_index(inplace=True, drop=True)

    return df, index


def resample(df, col, ratio, rng=None):
    if rng is None:
        rng = np.random.Generator(np.random.PCG64())
    print(rng.bit_generator.state)
    levels = sorted(set(df[col]))
    sdfs = []
    for l in levels:
        sel = df.index[df[col] == l]
        sz = max(1, int(len(sel) * ratio))
        sdfs += [df.iloc[rng.choice(sel, replace=False, size=sz)]]
    return pd.concat(sdfs)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Resample Gallup World Poll data")
    parser.add_argument("filename")
    args = parser.parse_args()

    filename = args.filename

    rng = np.random.Generator(np.random.MT19937(seed))
    rng.bit_generator.state = np.random.RandomState(seed).get_state()

    df, index = load_data(
        filename, dependent_vars, noncat_fixed_vars, cat_fixed_vars, cat_rnd_vars
    )
    df.drop(
        columns=["security_obj", "country_average_security_obj", "gini"], inplace=True
    )
    df = df.reindex(
        columns=[
            "id",
            "weight",
            "year",
            "country_code",
            "country_name",
            "country_year",
            "age_squared",
            "life_evaluation",
            "life_satisfaction",
            "positive_affect",
            "absence_negative_affect",
            "gender",
            "age",
            "relationship_status",
            "food",
            "housing",
            "health",
            "water",
            "air",
            "healthcare",
            "security",
            "social_support",
            "respect",
            "education",
            "interesting_activity",
            "recreation",
            "occupation",
            "freedom",
            "country_average_food",
            "country_average_housing",
            "country_average_health",
            "country_average_water",
            "country_average_air",
            "country_average_healthcare",
            "country_average_security",
            "country_average_social_support",
            "country_average_respect",
            "country_average_education",
            "country_average_interesting_activity",
            "country_average_recreation",
            "country_average_occupation",
            "country_average_freedom",
            "gdp",
            "growth",
            "personal_income",
            "relative_income",
            "social_protection",
            "welfare_state",
            "government_effectiveness",
            "democracy",
            "importance_rich",
            "religious",
            "altruistic_behaviour",
            "rating_economic_conditions",
            "confidence_institutions",
            "expected_economic_conditions",
            "satisfaction_standard_living",
        ]
    )

    for i in range(5, 56, 5):
        resample(df, "country_year", i / 100, rng).sort_index().convert_dtypes().to_csv(
            "resamples/data_level{0:02}.csv".format(i), index=False, na_rep="NA"
        )
