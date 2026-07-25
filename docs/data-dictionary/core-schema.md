# Core Data Model

## Teams

- team_id
- team_name
- team_abbreviation
- conference
- division
- primary_color
- secondary_color
- logo_path

## Players

- player_id
- player_name
- team_id
- position
- birth_date
- age
- height_inches
- weight_lbs
- roster_status

## Contracts

- contract_id
- player_id
- team_id
- season
- salary
- cap_hit
- guaranteed_amount
- contract_type
- option_type
- guarantee_date
- trade_bonus
- signed_date
- expiration_season

## Salary Cap

- season
- salary_cap
- luxury_tax
- first_apron
- second_apron
- minimum_team_salary

## Draft Assets

- asset_id
- team_id
- draft_year
- round
- original_team
- ownership_type
- protection
- conveys_to

## Future Scenarios

- scenario_id
- team_id
- scenario_name
- created_at
- description