The data will reside in the following tables (based largely on the fact that the data sources are quite different):

- community_demographics
- voter_demographics
- candidate_demographics
- elected_body_demographics

Initial thoughts on the columns in these tables are below. If we're going to later extend this beyond county commissions or look at sub-county district data, how does this need to be revised?

## community_demographics (based on ACS data)
- Year
- Jurisdiction type (e.g., county)
- Jurisdiction identifier (name)
- Total population (presumably voting age)
- Gender counts: 3 columns: male, female, non-binary (please correct if this is wrong)
- Race counts: [TBD]

## voter_demographics (based on voter registration data)
- Election year (for earlier years, as of election day; for current year, data should be updated regularly)
- Voting status (did they vote in this year's election?)
- Jurisdiction type (e.g., county)
- Jurisdiction identifier (name)
- Total population (presumably voting age)
- Gender counts: 3 columns: male, female, non-binary (please correct if this is wrong)
- Race counts: [TBD]

## candidate_demographics (based on BOE candidate lists)
- Election year
- Voting status (did they vote in this year's election?)
- Jurisdiction type (e.g., county)
- Jurisdiction identifier (name)
- Elected office
- Total population (presumably voting age)
- Gender counts: 3 columns: male, female, non-binary (please correct if this is wrong)
- Race counts: [TBD]

Note: The demographic data can be drawn from their voter registration, but we probably need to get candidate lists *with* home address in order to ensure a correct match.

## elected_body_demographics
- Election year (following the election)
- Jurisdiction type (e.g., county)
- Jurisdiction identifier (name)
- Elected office
- Total population (presumably voting age)
- Gender counts: 3 columns: male, female, non-binary (please correct if this is wrong)
- Race counts: [TBD]

