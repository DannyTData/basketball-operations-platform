# ------------------------------------------------------------
# Module: CBA Knowledge Base
# Thompson Basketball Intelligence
# Version 2
# ------------------------------------------------------------

# ============================================================
# DATA
# ============================================================

#' Create one normalized CBA knowledge entry
#' @noRd
tbi_cba_glossary_entry <- function(
    term,
    category,
    short_definition,
    front_office_impact,
    module,
    example,
    affects,
    related_terms,
    aliases = "",
    source_reference,
    verification_status = "Supported summary") {
  data.frame(
    term = term,
    category = category,
    short_definition = short_definition,
    front_office_impact = front_office_impact,
    module = module,
    example = example,
    affects = affects,
    related_terms = related_terms,
    aliases = aliases,
    source = "2023 NBA-NBPA CBA / NBA CBA 101",
    source_reference = source_reference,
    verification_status = verification_status,
    stringsAsFactors = FALSE
  )
}


#' Normalize a CBA term or alias for matching
#' @noRd
tbi_cba_normalize_term_key <- function(value) {
  value <- trimws(tolower(as.character(value)))
  trimws(gsub("[^a-z0-9]+", " ", value))
}

#' CBA knowledge-base data
#' @noRd
tbi_cba_glossary_data <- function() {
  glossary <- data.frame(
    term = c(
      "Salary Cap",
      "Team Salary",
      "Luxury Tax",
      "First Apron",
      "Second Apron",
      "Hard Cap",
      "Cap Hold",
      "Bird Exception",
      "Early Bird Exception",
      "Non-Bird Exception",
      "Non-Taxpayer Mid-Level Exception",
      "Taxpayer Mid-Level Exception",
      "Room Mid-Level Exception",
      "Bi-Annual Exception",
      "Minimum Player Salary Exception",
      "Traded Player Exception (TPE)",
      "Salary Aggregation",
      "Salary Matching",
      "Sign-and-Trade",
      "Base Year Compensation",
      "Trade Bonus / Trade Kicker",
      "Rookie Scale Contract",
      "Rookie-Scale Extension",
      "Designated Rookie Extension",
      "Veteran Extension",
      "Designated Veteran Extension",
      "Qualifying Offer",
      "Restricted Free Agent (RFA)",
      "Unrestricted Free Agent (UFA)",
      "Team Option",
      "Player Option",
      "Two-Way Contract",
      "Exhibit 10",
      "Waivers",
      "Stretch Provision",
      "Dead Money",
      "10-Day Contract",
      "Two-Way Conversion"
    ),
    
    category = c(
      "Cap Structure","Cap Structure","Cap Structure","Aprons","Aprons",
      "Cap Structure","Free Agency","Exceptions","Exceptions","Exceptions",
      "Exceptions","Exceptions","Exceptions","Exceptions","Exceptions",
      "Trades","Trades","Trades","Trades","Trades","Trades","Contracts",
      "Extensions","Extensions","Extensions","Extensions","Free Agency",
      "Free Agency","Free Agency","Contracts","Contracts","Contracts",
      "Contracts","Contracts","Contracts","Cap Structure","Contracts","Contracts"
    ),
    
    short_definition = c(
      "The collectively bargained team payroll threshold used to determine whether a team has cap room and which roster-building mechanisms are available.",
      "The salary amount charged to a team for CBA accounting purposes after applying contract, exception, cap-hold, and other CBA rules.",
      "A payroll threshold above the Salary Cap. Teams above it may owe tax payments and face additional roster-building consequences.",
      "A threshold above the luxury-tax level that activates additional restrictions for teams whose team salary exceeds it.",
      "A higher apron threshold that triggers the CBA's most restrictive team-building rules.",
      "A payroll ceiling that a team may not exceed after using certain transactions or exceptions that impose an apron-based limitation.",
      "A temporary amount that counts against team salary for certain free agents, draft picks, exceptions, and other roster rights until resolved.",
      "An exception allowing a team to exceed the Salary Cap to re-sign a qualifying veteran free agent after the required tenure with the team.",
      "An exception allowing an over-cap team to re-sign certain qualifying free agents with a shorter qualifying tenure than full Bird rights.",
      "An exception allowing a team to re-sign certain free agents who do not qualify for Bird or Early Bird rights.",
      "A mid-level salary exception generally available to qualifying over-cap teams that remain within the applicable apron restrictions.",
      "A smaller mid-level exception available to qualifying tax teams, subject to the CBA's apron restrictions.",
      "A mid-level exception available to qualifying teams that used salary-cap room.",
      "An exception that can allow a qualifying team to sign a player above the minimum salary, subject to usage and apron restrictions.",
      "An exception that allows teams to sign qualifying players to minimum-salary contracts even when they are over the Salary Cap.",
      "A CBA mechanism that can permit an over-cap team to acquire replacement salary in a trade or waiver claim within the applicable matching rules.",
      "Combining the outgoing salaries of multiple players for purposes of determining permitted incoming salary where the CBA allows aggregation.",
      "The trade rules that determine how much incoming salary an over-cap team may receive relative to the salary it sends out.",
      "A transaction in which a free agent signs a new contract with his prior team and is immediately traded to another team, subject to CBA requirements.",
      "A salary-accounting rule that can modify the outgoing salary amount assigned to certain players in sign-and-trade calculations.",
      "Contract language that can increase a player's compensation when he is traded, subject to CBA limits and allocation rules.",
      "The standard CBA salary structure for first-round draft picks, generally covering the player's first four NBA seasons with team-controlled option years.",
      "An extension of a first-round rookie-scale contract negotiated before the player reaches restricted free agency.",
      "A rookie-scale extension using the CBA's designated-player rules and limits.",
      "An extension that adds seasons to an eligible veteran player's existing contract under the applicable CBA rules.",
      "A veteran extension using the CBA's designated-veteran eligibility rules, including required service and award criteria.",
      "A required offer that can preserve a team's right of first refusal and make an eligible player a restricted free agent.",
      "A free agent whose prior team retains a right of first refusal if the applicable qualifying-offer requirements were met.",
      "A free agent who may sign with any team without his prior team possessing a right of first refusal.",
      "A contract year that becomes guaranteed only if the team exercises the option.",
      "A contract year that becomes guaranteed only if the player exercises the option.",
      "A contract allowing a player to move between an NBA team and its G League affiliate under the CBA's two-way rules.",
      "A contract attachment commonly used for training-camp deals that can include a bonus and can facilitate conversion to a two-way contract when eligible.",
      "The process through which a team terminates a player's contract and makes him available to be claimed by other teams before he clears waivers.",
      "A mechanism that can spread certain waived salary-cap charges across additional seasons rather than charging the full remaining amount on the original schedule.",
      "Salary-cap charges attributable to a player who is no longer on the active roster, including certain waived, stretched, or otherwise retained salary obligations.",
      "A short-term NBA contract available during the designated portion of the regular season, subject to CBA limits.",
      "The process of converting an eligible two-way player to a standard NBA contract."
    ),
    
    front_office_impact = c(
      "Determines whether the club operates with cap room or through exceptions and influences nearly every transaction strategy.",
      "This is the number the cap engine cares about. It may differ from cash paid or a player's headline salary.",
      "A tax-team designation can materially alter transaction costs and available exceptions.",
      "Crossing the First Apron can remove or limit certain transaction mechanisms and may hard-cap a team in specific situations.",
      "Crossing the Second Apron can sharply reduce trade, exception, and future roster-building flexibility.",
      "Before triggering a hard cap, basketball operations must model the entire season because the club cannot simply spend through the ceiling later.",
      "Cap holds preserve rights but consume room. Renouncing a hold may create space while giving up a valuable exception or free-agent right.",
      "Full Bird rights can be one of a team's most valuable retention tools because they can permit a significant over-cap re-signing.",
      "Useful for retaining players when full Bird rights have not yet been established.",
      "Provides limited retention flexibility when stronger Bird rights are unavailable.",
      "A major roster-building tool for over-cap clubs, but using it may interact with apron restrictions.",
      "Provides tax teams a narrower external signing tool than the non-taxpayer mid-level.",
      "Allows a team that used cap room to retain some additional signing flexibility after spending that room.",
      "Can create another signing path, but eligibility and apron consequences must be checked before use.",
      "Allows over-cap teams to fill roster spots without needing cap room or a larger exception.",
      "A TPE can preserve trade flexibility after salary is sent out, but expiration, aggregation, and apron rules matter.",
      "Aggregation can make larger acquisitions possible, but apron rules may prohibit it for certain teams.",
      "This is the core legal screen behind the Trade Intelligence module's incoming-versus-outgoing salary analysis.",
      "A powerful free-agency mechanism, but it creates restrictions for the receiving team and must satisfy contract and transaction rules.",
      "Important because the salary a team is credited with sending may differ from the player's new first-year salary.",
      "A trade kicker can affect matching math, cap charges, and the feasibility of a transaction.",
      "The rookie scale creates predictable early-career team control and cost certainty for first-round picks.",
      "An extension can eliminate future RFA uncertainty but also commits salary before the player's next market test.",
      "A designated rookie slot can provide maximum-level retention but uses one of the team's designated-player pathways.",
      "Veteran extensions can preserve talent and reduce free-agency risk, but eligibility, timing, salary, raise, and term rules must all be screened.",
      "This is the pathway commonly associated with the largest veteran extension opportunities; award and team-history eligibility must be verified.",
      "Failing to issue the required qualifying offer can eliminate restricted-free-agency control.",
      "RFA status gives the incumbent team leverage through matching rights but can create cap-hold and timing considerations.",
      "UFA status creates full market exposure and removes matching protection for the prior team.",
      "Team options preserve organizational control and can become important decision dates in roster planning.",
      "Player options create uncertainty because the player, not the team, controls whether the final season remains in the contract.",
      "Two-way slots expand the development pipeline without using a standard roster spot, subject to eligibility and service rules.",
      "Exhibit 10 deals are useful for camp competition, G League pipelines, and potential two-way conversion.",
      "Waiver timing, claim rights, guarantees, and cap treatment must be evaluated before releasing a player.",
      "Stretching salary can create short-term room while extending dead-money charges into future years.",
      "Dead money reduces usable payroll without providing active-roster production, making it a key flexibility signal.",
      "Useful for short-term injury or roster coverage after the CBA's 10-day signing window opens.",
      "Conversion can secure a developing player on a standard contract and changes roster-slot and salary treatment."
    ),
    
    module = c(
      "Cap Intelligence","Cap Intelligence","Cap Intelligence","Cap Intelligence","Cap Intelligence",
      "Cap Intelligence","Cap Intelligence","Cap Intelligence","Cap Intelligence","Cap Intelligence",
      "Cap Intelligence","Cap Intelligence","Cap Intelligence","Cap Intelligence","Cap Intelligence",
      "Trade Intelligence","Trade Intelligence","Trade Intelligence","Trade Intelligence","Trade Intelligence",
      "Trade Intelligence","Player Management","Extension Simulator","Extension Simulator","Extension Simulator",
      "Extension Simulator","Player Management","Player Management","Player Management","Player Management",
      "Player Management","Roster Intelligence","Roster Intelligence","Roster Intelligence","Cap Intelligence",
      "Cap Intelligence","Roster Intelligence","Roster Intelligence"
    ),
    
    example = c(
      "A team is below the Salary Cap and can use cap room to sign a free agent rather than relying on an exception.",
      "A player's cash salary is not necessarily identical to the amount that counts toward Team Salary.",
      "A club can be over the Salary Cap and also above the tax line, creating both a tax bill and additional roster-building consequences.",
      "A proposed signing or trade pushes team salary above the First Apron, so the front office must screen which transaction tools become restricted.",
      "A club above the Second Apron evaluates a trade and discovers that rules available to lower-payroll teams may no longer be available.",
      "A sign-and-trade or exception use can impose a ceiling that the team must remain under for the applicable period.",
      "A free agent remains unsigned, so his cap hold continues to occupy room until he signs, is renounced, or another CBA resolution occurs.",
      "An over-cap team re-signs its own qualifying veteran using Bird rights instead of needing cap room.",
      "A team retains a player who has enough qualifying tenure for Early Bird rights but not full Bird rights.",
      "A team can retain a player above the minimum using Non-Bird rights when stronger Bird rights are unavailable.",
      "An over-cap team uses the non-taxpayer mid-level to add a rotation player while monitoring apron consequences.",
      "A tax team uses the taxpayer mid-level as one of its limited external signing tools.",
      "A team first uses cap room, then uses the room mid-level to add another player.",
      "A qualifying club uses the bi-annual exception to add a role player without cap room.",
      "An over-cap club signs a veteran minimum player without needing cap space.",
      "A team sends out salary in a trade and creates a TPE that can later be used to absorb qualifying salary.",
      "A team combines two outgoing contracts to reach the salary needed for a larger incoming player, if aggregation is permitted.",
      "Boston sends out $30M and checks how much salary it can legally receive under its pre-trade cap status.",
      "A free agent agrees to join another team, but the transaction is structured as a sign-and-trade through his prior club.",
      "A newly signed player's outgoing salary for trade purposes is limited by BYC rules even though his new contract pays more.",
      "A player's contract contains a trade kicker that increases compensation when he is dealt, affecting trade math.",
      "A first-round pick signs the standard rookie-scale structure with team-controlled option seasons.",
      "A team extends a rookie-scale player before restricted free agency to remove future market uncertainty.",
      "A qualifying young star signs a designated rookie extension under the designated-player rules.",
      "A veteran becomes extension-eligible and the team compares extending now versus waiting for free agency.",
      "A qualifying star with the required service and award criteria is screened for a designated veteran extension.",
      "A team issues a qualifying offer before the deadline so the player enters restricted free agency rather than unrestricted free agency.",
      "Another team signs an RFA to an offer sheet, giving the incumbent club an opportunity to match under the applicable rules.",
      "A veteran reaches unrestricted free agency and can negotiate with any team without his prior club holding matching rights.",
      "A team exercises a team option to keep a player under contract for the option season.",
      "A player exercises his player option and remains under contract rather than entering free agency.",
      "A developing player splits time between the NBA roster and G League under a two-way contract.",
      "A training-camp player signs an Exhibit 10 arrangement that may support a later two-way or G League pathway.",
      "A team waives a player and must wait through the waiver process before he becomes a free agent if unclaimed.",
      "A team stretches a waived guarantee to reduce the immediate annual cap charge at the cost of future dead money.",
      "A waived player's remaining guaranteed salary continues to count against the cap even though he is no longer on the roster.",
      "A team with a short-term roster need signs a player to a 10-day contract during the eligible portion of the season.",
      "A two-way player earns a standard roster opportunity and is converted to an NBA contract."
    ),
    
    affects = c(
      "Cap Room|Trades|Free Agency|Exceptions",
      "Cap Room|Trades|Tax|Aprons",
      "Tax|Aprons|Exceptions|Trades",
      "Trades|Exceptions|Hard Cap|Free Agency",
      "Trades|Exceptions|Draft|Future Flexibility",
      "Trades|Free Agency|Exceptions|Roster Moves",
      "Cap Room|Free Agency|Bird Rights",
      "Free Agency|Retention|Cap Room",
      "Free Agency|Retention|Cap Room",
      "Free Agency|Retention",
      "Free Agency|Aprons|Hard Cap",
      "Free Agency|Tax|Aprons",
      "Free Agency|Cap Room",
      "Free Agency|Aprons",
      "Roster Moves|Free Agency|Cap",
      "Trades|Salary Matching|Flexibility",
      "Trades|Salary Matching|Aprons",
      "Trades|Aprons|Team Salary",
      "Trades|Free Agency|Hard Cap",
      "Trades|Salary Matching",
      "Trades|Salary Matching|Contract Value",
      "Draft|Contracts|Team Control",
      "Extensions|RFA|Future Payroll",
      "Extensions|Designated Players|Future Payroll",
      "Extensions|Future Payroll|Free Agency",
      "Extensions|Maximum Salary|Eligibility",
      "RFA|Free Agency|Cap Hold",
      "Free Agency|Offer Sheets|Retention",
      "Free Agency|Market Exposure",
      "Contracts|Team Control|Free Agency",
      "Contracts|Player Control|Free Agency",
      "Development|Roster Spots|G League",
      "Training Camp|Two-Way|G League",
      "Contracts|Roster Spots|Cap",
      "Waivers|Dead Money|Future Cap",
      "Cap|Waivers|Future Flexibility",
      "Roster Spots|Short-Term Depth",
      "Roster Spots|Development|Cap"
    ),
    
    related_terms = c(
      "Team Salary|Luxury Tax|First Apron|Second Apron|Cap Hold",
      "Salary Cap|Luxury Tax|First Apron|Second Apron|Hard Cap",
      "Salary Cap|First Apron|Second Apron|Taxpayer Mid-Level Exception",
      "Luxury Tax|Second Apron|Hard Cap|Non-Taxpayer Mid-Level Exception",
      "First Apron|Hard Cap|Salary Aggregation|Salary Matching",
      "First Apron|Second Apron|Sign-and-Trade|Non-Taxpayer Mid-Level Exception",
      "Salary Cap|Bird Exception|Restricted Free Agent (RFA)|Qualifying Offer",
      "Early Bird Exception|Non-Bird Exception|Cap Hold|Restricted Free Agent (RFA)",
      "Bird Exception|Non-Bird Exception|Cap Hold",
      "Bird Exception|Early Bird Exception|Cap Hold",
      "First Apron|Hard Cap|Taxpayer Mid-Level Exception|Room Mid-Level Exception",
      "Luxury Tax|Second Apron|Non-Taxpayer Mid-Level Exception",
      "Salary Cap|Non-Taxpayer Mid-Level Exception",
      "First Apron|Hard Cap|Non-Taxpayer Mid-Level Exception",
      "Salary Cap|Team Salary",
      "Salary Matching|Salary Aggregation|Second Apron",
      "Salary Matching|Traded Player Exception (TPE)|Second Apron",
      "Salary Aggregation|Traded Player Exception (TPE)|Second Apron",
      "Hard Cap|Base Year Compensation|Salary Matching",
      "Sign-and-Trade|Salary Matching",
      "Salary Matching|Base Year Compensation",
      "Rookie-Scale Extension|Team Option",
      "Rookie Scale Contract|Designated Rookie Extension|Restricted Free Agent (RFA)",
      "Rookie-Scale Extension|Veteran Extension",
      "Designated Veteran Extension|Unrestricted Free Agent (UFA)",
      "Veteran Extension|Maximum Salary",
      "Restricted Free Agent (RFA)|Cap Hold|Bird Exception",
      "Qualifying Offer|Bird Exception|Cap Hold",
      "Bird Exception|Cap Hold",
      "Player Option|Rookie Scale Contract",
      "Team Option|Unrestricted Free Agent (UFA)",
      "Exhibit 10|Two-Way Conversion",
      "Two-Way Contract|Two-Way Conversion",
      "Stretch Provision|Dead Money",
      "Waivers|Dead Money",
      "Waivers|Stretch Provision",
      "Waivers|Minimum Player Salary Exception",
      "Two-Way Contract|Exhibit 10"
    ),
    
    source = rep(
      "2023 NBA-NBPA CBA / NBA CBA 101",
      38
    ),
    
    stringsAsFactors = FALSE
  )

  glossary$aliases <- ""
  legacy_aliases <- c(
    "Salary Cap" = "Cap|Cap Line",
    "Team Salary" = "Payroll for CBA Purposes",
    "Luxury Tax" = "Tax Line|Luxury Tax Line",
    "First Apron" = "First Tax Apron",
    "Second Apron" = "Second Tax Apron",
    "Hard Cap" = "Hard-Capped|Hard-Cap Ceiling",
    "Cap Hold" = "Free-Agent Amount",
    "Bird Exception" = "Bird Rights|Full Bird Rights|Larry Bird Exception",
    "Early Bird Exception" = "Early Bird Rights",
    "Non-Bird Exception" = "Non-Bird Rights",
    "Non-Taxpayer Mid-Level Exception" = "Non-Taxpayer MLE|NTMLE",
    "Taxpayer Mid-Level Exception" = "Taxpayer MLE|TMLE",
    "Room Mid-Level Exception" = "Room Exception|Room MLE",
    "Bi-Annual Exception" = "BAE|Biannual Exception",
    "Minimum Player Salary Exception" = "Minimum Salary Exception|Minimum Exception",
    "Traded Player Exception (TPE)" = "TPE|Traded Player Exception",
    "Salary Aggregation" = "Aggregation|Aggregating Salaries",
    "Salary Matching" = "Trade Matching",
    "Sign-and-Trade" = "Sign and Trade|S-and-T",
    "Base Year Compensation" = "BYC",
    "Trade Bonus / Trade Kicker" = "Trade Bonus|Trade Kicker",
    "Rookie Scale Contract" = "Rookie Scale",
    "Rookie-Scale Extension" = "Rookie Extension|Rookie Scale Extension",
    "Designated Rookie Extension" = "Designated Rookie|Designated Rookie Scale Extension",
    "Veteran Extension" = "Veteran Contract Extension",
    "Designated Veteran Extension" = "Designated Veteran Extension|DVPE",
    "Qualifying Offer" = "QO",
    "Restricted Free Agent (RFA)" = "Restricted Free Agency|RFA",
    "Unrestricted Free Agent (UFA)" = "Unrestricted Free Agency|UFA",
    "Team Option" = "Club Option",
    "Player Option" = "PO",
    "Two-Way Contract" = "Two Way Contract|Two-Way",
    "Exhibit 10" = "Exhibit 10 Contract",
    "Waivers" = "Waiver Process",
    "Stretch Provision" = "Stretch|Stretch Waiver",
    "Dead Money" = "Dead Salary",
    "10-Day Contract" = "Ten-Day Contract",
    "Two-Way Conversion" = "Two Way Conversion"
  )
  glossary$aliases[match(names(legacy_aliases), glossary$term)] <-
    unname(legacy_aliases)
  glossary$source_reference <- paste0(
    "2023 CBA: ",
    glossary$category,
    " provisions; NBA CBA 101 overview"
  )
  glossary$verification_status <- "Supported summary"

  expansion <- do.call(
    rbind,
    list(
      tbi_cba_glossary_entry(
        "Tax Apron Terminology", "Aprons",
        "A general or historical shorthand for an apron threshold tied to tax-level team salary. The current agreement distinguishes the First Apron and Second Apron, so the applicable threshold must be identified explicitly.",
        "Front offices should avoid treating 'the apron' as one interchangeable line because the operative restriction depends on the season, transaction, and specific apron threshold.",
        "Cap Intelligence",
        "A planning memo that says only 'tax apron' is routed to the First Apron and Second Apron screens before a transaction decision is made.",
        "Tax|Aprons|Trades|Exceptions",
        "Luxury Tax|First Apron|Second Apron|Hard Cap",
        "Tax Apron|Apron",
        "2023 CBA: First Apron and Second Apron provisions",
        "Requires source verification"
      ),
      tbi_cba_glossary_entry(
        "Early Termination Option", "Contracts",
        "A contract provision that may allow a player to end the contract before its stated final season, subject to the option's negotiated terms and applicable CBA rules.",
        "An ETO creates a player-controlled decision date that can accelerate free agency and change projected team salary.",
        "Player Management",
        "A player with an ETO can choose whether to remain under the existing contract or reach free agency at the option window.",
        "Contracts|Free Agency|Future Payroll",
        "Player Option|Unrestricted Free Agent (UFA)|Cap Hold",
        "ETO|Early Termination",
        "2023 CBA: player contract option provisions"
      ),
      tbi_cba_glossary_entry(
        "Mid-Level Exception", "Exceptions",
        "A family of salary-cap exceptions that can allow an eligible over-cap or room team to sign players without equivalent cap space, with the available form depending on team salary and prior transactions.",
        "The team must identify the correct mid-level exception before committing salary because amount, term, availability, and apron consequences differ.",
        "Cap Intelligence",
        "A team screens its salary position before deciding whether the non-taxpayer, taxpayer, or room form of the mid-level exception is available.",
        "Free Agency|Aprons|Hard Cap|Exceptions",
        "Non-Taxpayer Mid-Level Exception|Taxpayer Mid-Level Exception|Room Mid-Level Exception",
        "MLE|Midlevel Exception",
        "2023 CBA: mid-level exception provisions; NBA CBA 101 exceptions overview"
      ),
      tbi_cba_glossary_entry(
        "Disabled Player Exception", "Exceptions",
        "An exception that may be granted after the required determination concerning a player who is substantially more likely than not to be unable to play through the specified period.",
        "It can provide a limited replacement-player mechanism, but it does not remove the injured player's salary and requires league approval and timing review.",
        "Cap Intelligence",
        "A club with a qualifying long-term injury requests league approval before treating the disabled player exception as an available signing or trade tool.",
        "Injuries|Exceptions|Roster Moves|Team Salary",
        "Hard Cap|Minimum Player Salary Exception|Traded Player Exception (TPE)",
        "DPE|Disabled Player",
        "2023 CBA: disabled player exception provisions"
      ),
      tbi_cba_glossary_entry(
        "Poison Pill Provision", "Trades",
        "A trade-salary treatment that can apply to certain players on rookie-scale extensions before the extension begins, causing the sending and receiving teams to use different salary amounts in matching analysis.",
        "A transaction may pass ordinary headline-salary math but fail or require restructuring once the applicable poison-pill amounts are used.",
        "Trade Intelligence",
        "A recently extended rookie-scale player is evaluated with the applicable sending-team and receiving-team salary treatment rather than one shared cap number.",
        "Trades|Salary Matching|Extensions",
        "Rookie-Scale Extension|Salary Matching|Base Year Compensation",
        "Poison Pill|PPP",
        "2023 CBA: trade treatment of rookie-scale extensions",
        "Requires source verification"
      ),
      tbi_cba_glossary_entry(
        "Minimum Team Salary / Salary Floor", "Cap Structure",
        "The minimum team-salary requirement a club must satisfy for the applicable season under the CBA's minimum team salary rules.",
        "Operating below the floor does not create unlimited flexibility; the front office must plan for the agreement's timing and allocation consequences.",
        "Cap Intelligence",
        "A rebuilding team tracks its projected salary against the minimum team salary while sequencing signings and trades.",
        "Team Salary|Cap Room|Roster Construction",
        "Salary Cap|Team Salary|Roster Charge",
        "Salary Floor|Minimum Team Salary|Team Salary Floor",
        "2023 CBA: minimum team salary provisions; NBA CBA 101 cap overview"
      ),
      tbi_cba_glossary_entry(
        "Guaranteed Salary", "Contracts",
        "Contract compensation that remains owed and counts as required under the applicable guarantee terms unless reduced or otherwise treated under a permitted CBA mechanism.",
        "Guarantees determine the real downside of waiving, trading, or retaining a player and are central to cap-risk analysis.",
        "Player Management",
        "A fully guaranteed remaining season continues to create a team obligation even if the player is waived.",
        "Contracts|Waivers|Dead Money|Team Salary",
        "Non-Guaranteed Salary|Partial Guarantee|Waivers|Dead Money",
        "Fully Guaranteed|Guarantee",
        "2023 CBA: player contract guarantee and waiver provisions"
      ),
      tbi_cba_glossary_entry(
        "Non-Guaranteed Salary", "Contracts",
        "Contract compensation that is not guaranteed at the relevant time and may become protected based on contract dates, league rules, or other agreed conditions.",
        "Non-guaranteed salary can provide flexibility, but decision dates and transaction treatment must be checked before assuming the amount can be removed.",
        "Player Management",
        "A team reviews a player's guarantee date before deciding whether to waive him or retain the contract into the season.",
        "Contracts|Waivers|Roster Decisions|Team Salary",
        "Guaranteed Salary|Partial Guarantee|Waivers",
        "Unguaranteed Salary|Non Guaranteed Salary",
        "2023 CBA: player contract guarantee provisions"
      ),
      tbi_cba_glossary_entry(
        "Partial Guarantee", "Contracts",
        "A guarantee structure under which only a stated portion of compensation is protected at the relevant time.",
        "The protected amount, future guarantee dates, and transaction treatment must be modeled separately from the contract's full headline salary.",
        "Player Management",
        "A contract may carry a partial guarantee now and become fully guaranteed on a later date if the player remains on the roster.",
        "Contracts|Waivers|Dead Money|Team Salary",
        "Guaranteed Salary|Non-Guaranteed Salary|Waivers",
        "Partially Guaranteed|Partial Guarantee Amount",
        "2023 CBA: player contract guarantee provisions"
      ),
      tbi_cba_glossary_entry(
        "Roster Charge", "Cap Structure",
        "A cap-room accounting charge used in specified circumstances when a team has fewer than the required number of players or roster charges for cap-room calculations.",
        "Available cap room can be lower than a simple salary total suggests because required roster charges remain in the calculation.",
        "Cap Intelligence",
        "A cap-room team with open roster spots includes the applicable roster charges before estimating spending power.",
        "Cap Room|Team Salary|Roster Construction",
        "Salary Cap|Minimum Team Salary / Salary Floor|Cap Hold",
        "Incomplete Roster Charge|Minimum Roster Charge",
        "2023 CBA: team salary and incomplete-roster charge provisions"
      ),
      tbi_cba_glossary_entry(
        "Supermax / Designated Veteran Terminology", "Extensions",
        "Common shorthand for the designated veteran contract or extension pathway that can permit qualifying players to receive the applicable higher maximum-salary treatment.",
        "The nickname is not an eligibility test; service, award, team-history, timing, salary, and designated-player requirements must be verified independently.",
        "Extension Simulator",
        "A star is not treated as extension-eligible merely because the market calls the opportunity a supermax; the designated-veteran criteria are screened first.",
        "Extensions|Maximum Salary|Eligibility|Future Payroll",
        "Designated Veteran Extension|Maximum Salary|Extension Eligibility",
        "Supermax|Super Max|Designated Veteran Terminology",
        "2023 CBA: designated veteran contract and extension provisions",
        "Requires source verification"
      ),
      tbi_cba_glossary_entry(
        "Maximum Salary", "Contracts",
        "The highest starting salary generally permitted for a player under the applicable maximum-salary rules, subject to service, prior salary, exceptions, and specialized eligibility provisions.",
        "Maximum-salary modeling must use the player's actual eligibility and the correct cap-year inputs rather than a generic percentage assumption.",
        "Extension Simulator",
        "Two players in the same free-agent class may have different maximum starting salaries because their service or specialized eligibility differs.",
        "Contracts|Free Agency|Extensions|Team Salary",
        "Maximum Extension|Designated Veteran Extension|Salary Cap",
        "Max Salary|Maximum Contract",
        "2023 CBA: maximum player salary provisions; NBA CBA 101 maximum salary overview"
      ),
      tbi_cba_glossary_entry(
        "Maximum Extension", "Extensions",
        "The greatest extension terms available to an eligible player under the applicable extension pathway, salary base, raise, term, and timing rules.",
        "A maximum current contract and a maximum extension are not interchangeable; the permissible extension depends on the player's existing contract and eligibility route.",
        "Extension Simulator",
        "The extension screen calculates the permitted starting salary, raises, and term for the player's specific pathway before comparing proposals.",
        "Extensions|Future Payroll|Eligibility",
        "Veteran Extension|Rookie-Scale Extension|Maximum Salary|Extension Eligibility",
        "Max Extension",
        "2023 CBA: rookie-scale and veteran extension provisions"
      ),
      tbi_cba_glossary_entry(
        "Extension Eligibility", "Extensions",
        "The set of player, contract, service, timing, team-history, option, and other requirements that must be satisfied before a particular extension pathway is available.",
        "Eligibility must be established before proposal value is analyzed; an attractive financial structure is irrelevant if the pathway is unavailable.",
        "Extension Simulator",
        "The club screens the player's contract timing and pathway requirements before enabling the corresponding proposal range.",
        "Extensions|Contracts|Timing|Team Control",
        "Rookie-Scale Extension|Veteran Extension|Designated Veteran Extension|Maximum Extension",
        "Extension Eligible|Eligibility Window",
        "2023 CBA: extension eligibility and timing provisions"
      ),
      tbi_cba_glossary_entry(
        "Over-38 Rule", "Contracts",
        "A deferred-compensation rule that can reallocate salary treatment for certain multi-year contracts extending beyond the specified age-related threshold.",
        "Long-term contracts for older players require a dedicated screen because the cap treatment may not follow the simple annual payment schedule.",
        "Cap Intelligence",
        "Before offering a multi-year contract to an older free agent, the club verifies whether the over-38 treatment applies to the proposed term.",
        "Contracts|Free Agency|Team Salary|Term",
        "Maximum Salary|Bird Exception|Unrestricted Free Agent (UFA)",
        "Over 38 Rule|Over-38",
        "2023 CBA: over-38 deferred compensation provisions",
        "Requires source verification"
      ),
      tbi_cba_glossary_entry(
        "Cash Considerations", "Trades",
        "Cash that may be included in an NBA trade within the agreement's permitted purposes and annual limits, without itself being player salary for salary-matching purposes.",
        "Cash can support transaction economics but cannot substitute for required salary matching or cure another legality failure.",
        "Trade Intelligence",
        "A team may include permitted cash in a trade while the player and draft-asset routing still must independently pass CBA validation.",
        "Trades|Transaction Value|League Limits",
        "Salary Matching|Draft Rights|Second-Round Pick",
        "Cash in Trade|Trade Cash",
        "2023 CBA: cash payment provisions for trades",
        "Requires source verification"
      ),
      tbi_cba_glossary_entry(
        "Draft Rights", "Draft Assets",
        "A team's CBA-recognized rights to negotiate with or sign a drafted player whose NBA rights remain controlled by that team, subject to the applicable draft and contract rules.",
        "Draft rights can be routed in transactions as an asset, but they are distinct from a future draft pick and require unique-player and ownership verification.",
        "Draft Intelligence",
        "A team trades the NBA rights to a previously drafted player rather than trading a future selection.",
        "Draft|Trades|Roster Rights",
        "First-Round Pick|Second-Round Pick|Cash Considerations",
        "Player Draft Rights|NBA Draft Rights",
        "2023 CBA: draft rights and trade provisions"
      ),
      tbi_cba_glossary_entry(
        "First-Round Pick", "Draft Assets",
        "A selection in the first round of the NBA Draft, carrying the applicable rookie-scale contract framework and any ownership, protection, and conveyance terms attached to the asset.",
        "First-round picks combine player-acquisition value, predictable contract structure, Stepien constraints, and future optionality.",
        "Draft Intelligence",
        "A protected future first-round pick is modeled with its ownership year, protection schedule, and possible conveyance outcomes.",
        "Draft|Trades|Rookie Scale|Future Control",
        "Rookie Scale Contract|Pick Protection|Stepien Rule|Conveyance",
        "First Round Pick|FRP|1st-Round Pick",
        "2023 CBA: NBA Draft and first-round pick provisions"
      ),
      tbi_cba_glossary_entry(
        "Second-Round Pick", "Draft Assets",
        "A selection in the second round of the NBA Draft, subject to the applicable ownership, trade, contract, and conveyance rules.",
        "Second-round picks provide flexible development and transaction value but must still be tracked as unique assets with verified ownership.",
        "Draft Intelligence",
        "A team routes a future second-round selection to a trade partner while retaining its other picks in the same draft.",
        "Draft|Trades|Development|Future Control",
        "First-Round Pick|Pick Protection|Conveyance|Cash Considerations",
        "Second Round Pick|SRP|2nd-Round Pick",
        "2023 CBA: NBA Draft and second-round pick provisions"
      ),
      tbi_cba_glossary_entry(
        "Pick Protection", "Draft Assets",
        "A condition that determines whether a traded draft pick conveys in a given draft position or remains with the original team under the agreed protection schedule.",
        "Protection changes the probability, timing, and value of conveyance and must be modeled together with rollover or fallback obligations.",
        "Draft Intelligence",
        "A top-10-protected pick remains with the original team when it lands inside the protected range and follows the documented next-step obligation.",
        "Draft|Trades|Optionality|Conveyance",
        "First-Round Pick|Pick Obligation|Conveyance|Pick Swap",
        "Protected Pick|Draft Protection",
        "2023 CBA: draft-pick trade and protection provisions"
      ),
      tbi_cba_glossary_entry(
        "Pick Swap", "Draft Assets",
        "A contractual draft right allowing one team to exchange specified draft positions with another team when the documented conditions are met.",
        "A swap is conditional control rather than outright ownership and must be evaluated against both teams' picks and any existing obligations.",
        "Draft Intelligence",
        "A team exercises a swap only when the other team's pick is more favorable under the recorded swap terms.",
        "Draft|Trades|Optionality|Asset Routing",
        "Pick Protection|Conveyance|Pick Obligation|First-Round Pick",
        "Swap Rights|Draft Pick Swap",
        "2023 CBA: draft-pick swap and trade provisions"
      ),
      tbi_cba_glossary_entry(
        "Stepien Rule", "Draft Assets",
        "The restriction commonly described as preventing a team from leaving itself without a future first-round pick in consecutive future drafts, evaluated using the agreement's actual pick-availability rules.",
        "A proposed pick trade must be screened across the relevant future drafts, protections, swaps, and existing obligations rather than by counting headline picks alone.",
        "Draft Intelligence",
        "A team cannot approve a future first-round route until its remaining draft control passes the Stepien screen.",
        "Draft|Trades|Future Control|Pick Obligations",
        "First-Round Pick|Seven-Year Rule|Pick Protection|Pick Obligation",
        "Ted Stepien Rule|Stepien",
        "2023 CBA: future first-round draft-pick trade restrictions",
        "Requires source verification"
      ),
      tbi_cba_glossary_entry(
        "Seven-Year Rule", "Draft Assets",
        "The limit commonly used to describe how far into future NBA drafts a team may trade draft-pick rights under the applicable agreement and league calendar.",
        "Transaction planning must anchor the tradable horizon to the current league year and verified draft calendar before routing a distant pick.",
        "Draft Intelligence",
        "A team verifies that a proposed distant future pick falls inside the currently permitted trade horizon.",
        "Draft|Trades|Future Control|Calendar",
        "Stepien Rule|First-Round Pick|Pick Obligation",
        "Seven Year Rule|7-Year Rule",
        "2023 CBA: future draft-pick trade horizon provisions",
        "Requires source verification"
      ),
      tbi_cba_glossary_entry(
        "Conveyance", "Draft Assets",
        "The transfer of a draft asset to the receiving team when the recorded ownership, protection, and timing conditions are satisfied.",
        "Conveyance timing determines when an asset becomes usable and whether rollover or fallback obligations remain outstanding.",
        "Draft Intelligence",
        "A protected pick conveys when its draft position falls outside the protected range specified in the obligation.",
        "Draft|Trades|Pick Ownership|Timing",
        "Pick Protection|Pick Obligation|First-Round Pick|Second-Round Pick",
        "Pick Conveyance|Conveys",
        "2023 CBA: draft-pick trade and conveyance provisions"
      ),
      tbi_cba_glossary_entry(
        "Pick Obligation", "Draft Assets",
        "An outstanding commitment created by a prior draft transaction, including the asset, season, protection, rollover, swap, or fallback terms that still constrain team control.",
        "Existing obligations must be resolved before a team can safely route the same asset or a conflicting future pick in another transaction.",
        "Draft Intelligence",
        "A previously protected first-round commitment blocks a conflicting outgoing pick until its conveyance path is known.",
        "Draft|Trades|Asset Control|Duplicate Prevention",
        "Pick Protection|Conveyance|Stepien Rule|Pick Swap",
        "Draft Obligation|Owed Pick",
        "2023 CBA: draft-pick trade obligations and protection provisions"
      ),
      tbi_cba_glossary_entry(
        "Reacquisition Restrictions", "Transaction Restrictions",
        "Restrictions that can prevent a team from reacquiring a player it previously traded or waived until the applicable CBA conditions and waiting periods are satisfied.",
        "Player routing must include transaction history; salary matching alone cannot make an otherwise prohibited reacquisition legal.",
        "Trade Intelligence",
        "A team screens a former player's transaction history before allowing him to return through a trade or waiver claim.",
        "Trades|Waivers|Transaction History|Player Routing",
        "Recently Traded Restriction|Waivers|Salary Matching",
        "Reacquisition Rule|Reacquiring a Player",
        "2023 CBA: player reacquisition restrictions",
        "Requires source verification"
      ),
      tbi_cba_glossary_entry(
        "Recently Traded Restriction", "Transaction Restrictions",
        "A transaction-timing restriction that may limit how or when a recently acquired player can be aggregated or traded again under the applicable CBA rules.",
        "The engine must track acquisition timing and transaction context before permitting a second trade or salary aggregation.",
        "Trade Intelligence",
        "A recently acquired player is screened for any waiting period or aggregation restriction before being included in another transaction.",
        "Trades|Aggregation|Transaction Timing",
        "Salary Aggregation|Reacquisition Restrictions|Recently Signed Restriction",
        "Recently Traded Player|Trade Waiting Period",
        "2023 CBA: recently traded player and aggregation timing provisions",
        "Requires source verification"
      ),
      tbi_cba_glossary_entry(
        "Recently Signed Restriction", "Transaction Restrictions",
        "A transaction-timing restriction that may prevent or limit trading a recently signed player until the applicable date and contract conditions are satisfied.",
        "Signing date, signing method, contract type, and any specialized rule must be recorded before the player is considered trade-eligible.",
        "Trade Intelligence",
        "A newly signed player remains unavailable in the trade-routing screen until the applicable eligibility date is verified.",
        "Trades|Contracts|Transaction Timing|Signings",
        "Sign-and-Trade|Base Year Compensation|Recently Traded Restriction",
        "Recently Signed Player|Signing Trade Restriction",
        "2023 CBA: recently signed player trade restrictions",
        "Requires source verification"
      )
    )
  )

  glossary <- rbind(glossary, expansion)
  rownames(glossary) <- NULL
  glossary
}


#' Build the normalized alias-to-canonical CBA lookup
#' @noRd
tbi_cba_glossary_alias_index <- function(glossary = tbi_cba_glossary_data()) {
  canonical_terms <- as.character(glossary$term)
  keys <- tbi_cba_normalize_term_key(canonical_terms)
  values <- canonical_terms

  for (i in seq_len(nrow(glossary))) {
    aliases <- as.character(glossary$aliases[[i]])
    if (is.na(aliases) || !nzchar(trimws(aliases))) {
      next
    }

    aliases <- trimws(strsplit(aliases, "\\|", perl = TRUE)[[1]])
    aliases <- aliases[nzchar(aliases)]
    keys <- c(keys, tbi_cba_normalize_term_key(aliases))
    values <- c(values, rep(canonical_terms[[i]], length(aliases)))
  }

  conflicts <- split(values, keys)
  conflicts <- conflicts[vapply(conflicts, function(x) length(unique(x)) > 1L, logical(1))]
  if (length(conflicts)) {
    stop(
      paste0(
        "CBA alias maps to multiple canonical terms: ",
        paste(names(conflicts), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  keep <- !duplicated(keys)
  stats::setNames(values[keep], keys[keep])
}


#' Resolve a canonical CBA term or alias
#' @noRd
tbi_cba_resolve_term <- function(term, glossary = tbi_cba_glossary_data()) {
  term <- as.character(term)
  if (!length(term) || is.na(term[[1]]) || !nzchar(trimws(term[[1]]))) {
    return(NA_character_)
  }

  key <- tbi_cba_normalize_term_key(term[[1]])
  index <- tbi_cba_glossary_alias_index(glossary)
  if (!key %in% names(index)) {
    return(NA_character_)
  }

  unname(index[[key]])
}


#' Filter the CBA glossary using the public search contract
#' @noRd
tbi_cba_filter_glossary <- function(
    glossary,
    search_value = "",
    category = "All Categories") {
  d <- glossary
  category <- as.character(category)
  search_value <- trimws(tolower(as.character(search_value)))

  if (length(category) && !identical(category[[1]], "All Categories")) {
    d <- d[d$category == category[[1]], , drop = FALSE]
  }

  if (length(search_value) && nzchar(search_value[[1]]) && nrow(d)) {
    searchable_fields <- intersect(
      c(
        "term", "category", "short_definition", "front_office_impact",
        "example", "affects", "related_terms", "aliases", "module",
        "source", "source_reference", "verification_status"
      ),
      names(d)
    )
    haystack <- do.call(paste, c(d[searchable_fields], sep = " "))
    keep <- grepl(search_value[[1]], tolower(haystack), fixed = TRUE)
    d <- d[keep, , drop = FALSE]
  }

  d
}


# ============================================================
# UI
# ============================================================

#' CBA Knowledge Base UI
#'
#' @param id Internal module ID.
#' @noRd
mod_cba_glossary_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    id = ns("root"),
    class = "tbi-module-page cba-kb-page",
    
    shiny::tags$style(
      shiny::HTML(
        "
        .cba-kb-page {
          display:grid;
          gap:12px;
        }

        .cba-kb-hero {
          padding:18px 20px;
          display:grid;
          grid-template-columns:minmax(0,1fr) auto;
          gap:18px;
          align-items:end;
          border:1px solid rgba(96,165,250,.20);
          border-radius:14px;
          background:
            radial-gradient(circle at 84% 20%,rgba(59,130,246,.08),transparent 28%),
            linear-gradient(145deg,rgba(16,28,45,.98),rgba(10,20,34,.98));
        }

        .cba-kb-eyebrow {
          margin-bottom:5px;
          color:#6f8fb8;
          font-size:.54rem;
          font-weight:900;
          letter-spacing:.11em;
          text-transform:uppercase;
        }

        .cba-kb-title {
          margin:0 !important;
          color:#f6f8fb !important;
          font-size:1.72rem !important;
          font-weight:800 !important;
          letter-spacing:-.035em !important;
        }

        .cba-kb-subtitle {
          max-width:820px;
          margin:6px 0 0;
          color:#8da0b7;
          font-size:.67rem;
          line-height:1.5;
        }

        .cba-kb-count {
          min-width:94px;
          padding:10px 13px;
          border:1px solid rgba(96,165,250,.20);
          border-radius:10px;
          background:rgba(59,130,246,.045);
          text-align:center;
        }

        .cba-kb-count strong {
          display:block;
          color:#67a9ff;
          font-size:1.18rem;
        }

        .cba-kb-count span {
          display:block;
          margin-top:3px;
          color:#73869e;
          font-size:.46rem;
          font-weight:850;
          letter-spacing:.08em;
          text-transform:uppercase;
        }

        .cba-kb-search-panel {
          padding:13px 14px 14px;
          display:grid;
          grid-template-columns:1fr;
          gap:10px;
          border-bottom:1px solid rgba(148,163,184,.09);
          background:rgba(8,18,31,.42);
        }

        .cba-kb-search-panel .form-group {
          margin-bottom:0 !important;
        }

        .cba-kb-search-panel label {
          color:#74869d !important;
          font-size:.49rem !important;
          font-weight:900 !important;
          letter-spacing:.08em !important;
          text-transform:uppercase;
        }

        .cba-kb-search-panel .form-control,
        .cba-kb-search-panel .selectize-input {
          min-height:39px !important;
          border-color:rgba(148,163,184,.16) !important;
          border-radius:8px !important;
          background:#101b2b !important;
          color:#edf3f9 !important;
          font-size:.62rem !important;
        }

        .cba-kb-layout {
          display:grid;
          grid-template-columns:minmax(280px,320px) minmax(0,1fr);
          gap:14px;
          align-items:start;
          transition:grid-template-columns .18s ease-out;
        }

        .cba-kb-index-panel {
          min-width:0;
          position:sticky;
          top:12px;
          transition:width .18s ease-out;
        }

        .cba-kb-workspace-panel {
          min-width:0;
        }

        .cba-kb-index-toggle,
        .cba-kb-mobile-index-toggle,
        .cba-kb-index-close {
          min-height:34px;
          padding:7px 10px;
          cursor:pointer;
          border:1px solid rgba(96,165,250,.20);
          border-radius:8px;
          color:#9fc5f3;
          background:rgba(59,130,246,.055);
          font-size:.51rem;
          font-weight:850;
          letter-spacing:.02em;
        }

        .cba-kb-index-toggle:hover,
        .cba-kb-mobile-index-toggle:hover,
        .cba-kb-index-close:hover {
          border-color:rgba(96,165,250,.40);
          color:#d8eafe;
          background:rgba(59,130,246,.10);
        }

        .cba-kb-index-toggle:focus-visible,
        .cba-kb-mobile-index-toggle:focus-visible,
        .cba-kb-index-close:focus-visible {
          outline:2px solid #69a9ff;
          outline-offset:2px;
        }

        .cba-kb-index-toggle {
          flex:0 0 auto;
        }

        .cba-kb-index-close {
          display:none;
        }

        .cba-kb-mobile-index-toggle {
          display:none;
          width:max-content;
        }

        .cba-kb-page.cba-index-collapsed .cba-kb-layout {
          grid-template-columns:56px minmax(0,1fr);
        }

        .cba-kb-page.cba-index-collapsed .cba-kb-index-panel
          :is(.cba-kb-panel-head strong,.cba-kb-panel-head > span,.cba-kb-search-panel,.cba-kb-list) {
          display:none !important;
        }

        .cba-kb-page.cba-index-collapsed .cba-kb-index-panel .cba-kb-panel-head {
          min-height:56px;
          padding:0;
          justify-content:center;
          border-bottom:0;
        }

        .cba-kb-page.cba-index-collapsed .cba-kb-index-toggle {
          width:36px;
          padding:7px 0;
        }

        .cba-kb-page.cba-index-collapsed .cba-kb-index-toggle-label {
          display:none;
        }

        .cba-kb-page.cba-index-collapsed .cba-kb-index-toggle-icon {
          transform:rotate(180deg);
        }

        .cba-kb-panel {
          overflow:hidden;
          border:1px solid rgba(148,163,184,.10);
          border-radius:13px;
          background:
            linear-gradient(145deg,rgba(17,28,44,.96),rgba(10,20,34,.98));
        }

        .cba-kb-panel-head {
          min-height:52px;
          padding:0 15px;
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:10px;
          border-bottom:1px solid rgba(148,163,184,.09);
        }

        .cba-kb-panel-head strong {
          color:#eaf0f7;
          font-size:.75rem;
        }

        .cba-kb-panel-head span {
          color:#71859e;
          font-size:.47rem;
          font-weight:800;
          letter-spacing:.07em;
          text-transform:uppercase;
        }

        .cba-kb-list {
          max-height:calc(100vh - 290px);
          min-height:380px;
          padding:9px;
          overflow:auto;
          overscroll-behavior:contain;
        }

        .cba-kb-index-group + .cba-kb-index-group {
          margin-top:13px;
          padding-top:11px;
          border-top:1px solid rgba(148,163,184,.09);
        }

        .cba-kb-index-group-title {
          margin:0 2px 7px;
          color:#6f88a7;
          font-size:.48rem;
          font-weight:900;
          letter-spacing:.08em;
          text-transform:uppercase;
        }

        .cba-kb-term-card {
          width:100%;
          margin-bottom:7px;
          padding:10px 11px;
          cursor:pointer;
          border:1px solid rgba(96,165,250,.12);
          border-radius:9px;
          background:rgba(11,25,42,.55);
          transition:
            transform .12s ease,
            border-color .12s ease,
            background .12s ease;
        }

        .cba-kb-term-card:hover {
          transform:translateY(-1px);
          border-color:rgba(96,165,250,.30);
          background:rgba(20,39,64,.72);
        }

        .cba-kb-term-card.selected {
          border-color:#579cff;
          background:rgba(27,51,83,.78);
          box-shadow:0 0 0 1px rgba(87,156,255,.12);
        }

        .cba-kb-term-top {
          display:flex;
          justify-content:space-between;
          align-items:center;
          gap:8px;
        }

        .cba-kb-term-name {
          color:#eef4fa;
          font-size:.66rem;
          font-weight:830;
        }

        .cba-kb-term-category {
          flex:0 0 auto;
          padding:3px 6px;
          border:1px solid rgba(96,165,250,.15);
          border-radius:999px;
          color:#78a6df;
          background:rgba(59,130,246,.045);
          font-size:.41rem;
          font-weight:850;
          letter-spacing:.05em;
          text-transform:uppercase;
        }

        .cba-kb-preview {
          margin-top:5px;
          color:#7f91a8;
          font-size:.53rem;
          line-height:1.42;
        }

        .cba-kb-empty {
          padding:30px 18px;
          color:#71849d;
          font-size:.61rem;
          text-align:center;
        }

        .cba-kb-detail-body {
          padding:18px;
          display:grid;
          grid-template-columns:minmax(0,1.14fr) minmax(280px,.86fr);
          grid-template-areas:
            'category category'
            'title title'
            'meaning affects'
            'impact related'
            'example used'
            'rule-impact rule-impact'
            'source source';
          column-gap:22px;
          align-items:start;
        }

        .cba-kb-detail-body > [data-cba-detail-block] {
          min-width:0;
        }

        .cba-kb-detail-body > [data-cba-detail-block='category'] { grid-area:category; }
        .cba-kb-detail-body > [data-cba-detail-block='title'] { grid-area:title; }
        .cba-kb-detail-body > [data-cba-detail-block='meaning'] { grid-area:meaning; }
        .cba-kb-detail-body > [data-cba-detail-block='impact'] { grid-area:impact; }
        .cba-kb-detail-body > [data-cba-detail-block='example'] { grid-area:example; }
        .cba-kb-detail-body > [data-cba-detail-block='affects'] { grid-area:affects; }
        .cba-kb-detail-body > [data-cba-detail-block='related'] { grid-area:related; }
        .cba-kb-detail-body > [data-cba-detail-block='used'] { grid-area:used; }
        .cba-kb-detail-body > [data-cba-detail-block='rule-impact'] { grid-area:rule-impact; }
        .cba-kb-detail-body > [data-cba-detail-block='source'] { grid-area:source; }

        .cba-kb-category {
          color:#69a9ff;
          font-size:.49rem;
          font-weight:900;
          letter-spacing:.10em;
          text-transform:uppercase;
        }

        .cba-kb-term-title-row {
          display:flex;
          align-items:flex-start;
          justify-content:space-between;
          gap:12px;
        }

        .cba-kb-term-title {
          margin:5px 0 2px;
          color:#f5f8fc;
          font-size:1.42rem;
          font-weight:820;
          letter-spacing:-.03em;
        }

        .cba-kb-module {
          color:#7890ad;
          font-size:.54rem;
          font-weight:750;
        }

        .cba-kb-favorite-btn {
          flex:0 0 auto;
          margin-top:3px;
          padding:6px 9px;
          cursor:pointer;
          border:1px solid rgba(148,163,184,.14);
          border-radius:8px;
          color:#788da8;
          background:rgba(255,255,255,.015);
          font-size:.48rem;
          font-weight:850;
        }

        .cba-kb-favorite-btn.active {
          border-color:rgba(245,158,11,.25);
          color:#f2b347;
          background:rgba(245,158,11,.05);
        }

        .cba-kb-section {
          margin-top:17px;
          padding-top:14px;
          border-top:1px solid rgba(148,163,184,.09);
        }

        .cba-kb-label {
          margin-bottom:6px;
          color:#71859e;
          font-size:.48rem;
          font-weight:900;
          letter-spacing:.09em;
          text-transform:uppercase;
        }

        .cba-kb-alias-label {
          margin-top:13px;
        }

        .cba-kb-text {
          max-width:72ch;
          margin:0;
          color:#c4ceda;
          font-size:.68rem;
          line-height:1.60;
        }

        .cba-kb-impact {
          margin-top:12px;
          padding:12px;
          border:1px solid rgba(52,211,153,.15);
          border-radius:9px;
          background:rgba(16,185,129,.045);
        }

        .cba-kb-impact .cba-kb-label {
          color:#43d9a4;
        }

        .cba-kb-example {
          margin-top:12px;
          padding:12px;
          border:1px solid rgba(96,165,250,.14);
          border-radius:9px;
          background:rgba(59,130,246,.04);
        }

        .cba-kb-example .cba-kb-label {
          color:#63a7ff;
        }

        .cba-kb-chip-wrap {
          display:flex;
          flex-wrap:wrap;
          gap:6px;
        }

        .cba-kb-chip {
          padding:4px 7px;
          border:1px solid rgba(96,165,250,.14);
          border-radius:999px;
          color:#8eb2dd;
          background:rgba(59,130,246,.04);
          font-size:.45rem;
          font-weight:800;
        }

        .cba-kb-related {
          cursor:pointer;
        }

        .cba-kb-related:hover {
          border-color:#579cff;
          color:#bdd7fa;
          background:rgba(59,130,246,.08);
        }


        .cba-kb-rule-impact {
          margin-top:17px;
          padding-top:14px;
          border-top:1px solid rgba(148,163,184,.09);
        }

        .cba-kb-impact-grid {
          display:grid;
          grid-template-columns:repeat(2,minmax(0,1fr));
          gap:7px;
        }

        .cba-kb-impact-module {
          min-height:42px;
          padding:8px 9px;
          display:flex;
          align-items:center;
          gap:8px;
          border:1px solid rgba(148,163,184,.09);
          border-radius:8px;
          color:#657b94;
          background:rgba(255,255,255,.010);
          font-size:.49rem;
          font-weight:800;
          line-height:1.25;
        }

        .cba-kb-impact-module.active {
          border-color:rgba(96,165,250,.23);
          color:#a8c9f0;
          background:rgba(59,130,246,.055);
        }

        .cba-kb-impact-check {
          width:18px;
          height:18px;
          flex:0 0 18px;
          display:grid;
          place-items:center;
          border:1px solid rgba(148,163,184,.12);
          border-radius:50%;
          color:#526982;
          font-size:.43rem;
          font-weight:900;
        }

        .cba-kb-impact-module.active .cba-kb-impact-check {
          border-color:rgba(52,211,153,.24);
          color:#42d7a2;
          background:rgba(16,185,129,.055);
        }

        .cba-kb-source {
          margin-top:14px;
          padding:10px 11px;
          border:1px solid rgba(148,163,184,.08);
          border-radius:8px;
          background:rgba(255,255,255,.012);
          color:#71839a;
          font-size:.50rem;
          line-height:1.45;
        }

        .cba-kb-source strong {
          color:#9badc2;
        }

        .cba-kb-source-status {
          display:inline-flex;
          margin-top:7px;
          padding:3px 7px;
          border:1px solid rgba(52,211,153,.18);
          border-radius:999px;
          color:#77d9b6;
          background:rgba(16,185,129,.045);
          font-size:.44rem;
          font-weight:850;
        }

        .cba-kb-source-status.requires-verification {
          border-color:rgba(245,158,11,.22);
          color:#e6b45c;
          background:rgba(245,158,11,.05);
        }

        .cba-kb-source-note {
          margin-top:7px;
        }

        .cba-kb-history {
          display:grid;
          grid-template-columns:repeat(2,minmax(0,1fr));
          gap:12px;
        }

        .cba-kb-mini-list {
          padding:11px;
        }

        .cba-kb-mini-title {
          margin-bottom:8px;
          color:#758ba7;
          font-size:.48rem;
          font-weight:900;
          letter-spacing:.09em;
          text-transform:uppercase;
        }

        .cba-kb-mini-empty {
          color:#667b95;
          font-size:.52rem;
        }

        .cba-kb-mini-link {
          display:block;
          margin:0 0 5px;
          padding:6px 7px;
          cursor:pointer;
          border:1px solid rgba(96,165,250,.09);
          border-radius:7px;
          color:#9fb8d5;
          background:rgba(255,255,255,.012);
          font-size:.51rem;
          font-weight:750;
        }

        .cba-kb-mini-link:hover {
          border-color:rgba(96,165,250,.26);
          color:#d2e5fb;
        }

        .cba-kb-note {
          padding:10px 14px;
          border:1px solid rgba(245,158,11,.12);
          border-radius:10px;
          color:#8c9aae;
          background:rgba(245,158,11,.025);
          font-size:.52rem;
          line-height:1.5;
        }

        .cba-kb-note strong {
          color:#d6a555;
        }

        .cba-kb-drawer-scrim {
          display:none;
        }

        @media(max-width:1250px) {
          .cba-kb-detail-body {
            grid-template-columns:1fr;
            grid-template-areas:
              'category'
              'title'
              'meaning'
              'impact'
              'example'
              'affects'
              'related'
              'used'
              'rule-impact'
              'source';
          }
        }

        @media(max-width:1000px) {
          .cba-kb-layout {
            display:block;
          }

          .cba-kb-mobile-index-toggle {
            display:inline-flex;
            align-items:center;
            gap:7px;
          }

          .cba-kb-index-panel {
            width:min(360px,calc(100vw - 28px));
            max-height:calc(100vh - 28px);
            position:fixed;
            top:14px;
            left:14px;
            z-index:51;
            transform:translateX(calc(-100% - 28px));
            transition:transform .2s ease-out;
            box-shadow:0 6px 8px rgba(0,0,0,.34);
          }

          .cba-kb-page.cba-index-open .cba-kb-index-panel {
            transform:translateX(0);
          }

          .cba-kb-index-panel .cba-kb-panel-head {
            padding-right:10px;
          }

          .cba-kb-index-toggle {
            display:none;
          }

          .cba-kb-index-close {
            display:inline-flex;
            align-items:center;
          }

          .cba-kb-list {
            max-height:calc(100vh - 210px);
            min-height:260px;
          }

          .cba-kb-drawer-scrim {
            display:block;
            position:fixed;
            inset:0;
            z-index:50;
            pointer-events:none;
            opacity:0;
            background:rgba(3,8,15,.70);
            transition:opacity .2s ease-out;
          }

          .cba-kb-page.cba-index-open .cba-kb-drawer-scrim {
            pointer-events:auto;
            opacity:1;
          }
        }

        @media(max-width:650px) {
          .cba-kb-hero,
          .cba-kb-history {
            grid-template-columns:1fr;
          }

          .cba-kb-count {
            width:100%;
          }

          .cba-kb-detail-body {
            padding:15px;
          }

          .cba-kb-impact-grid {
            grid-template-columns:1fr;
          }

          .cba-kb-term-title-row {
            align-items:stretch;
            flex-direction:column;
          }

          .cba-kb-favorite-btn {
            width:max-content;
          }
        }

        @media(prefers-reduced-motion:reduce) {
          .cba-kb-layout,
          .cba-kb-index-panel,
          .cba-kb-drawer-scrim,
          .cba-kb-term-card {
            transition:none !important;
          }
        }
        "
      )
    ),
    
    shiny::tags$script(
      shiny::HTML(
        sprintf(
          "
          (function() {
            var page = document.getElementById('%s');
            if (!page || page.dataset.cbaKnowledgeBound === 'true') {
              return;
            }
            page.dataset.cbaKnowledgeBound = 'true';

            function compactIndex() {
              return window.matchMedia('(max-width:1000px)').matches;
            }

            function syncIndexButtons() {
              var expanded = compactIndex()
                ? page.classList.contains('cba-index-open')
                : !page.classList.contains('cba-index-collapsed');
              page.querySelectorAll('[data-cba-index-toggle]').forEach(function(button) {
                button.setAttribute('aria-expanded', expanded ? 'true' : 'false');
              });
            }

            function closeCompactIndex() {
              page.classList.remove('cba-index-open');
              syncIndexButtons();
            }

            page.addEventListener('click', function(e) {
              var toggle = e.target.closest('[data-cba-index-toggle]');
              if (toggle) {
                if (compactIndex()) {
                  page.classList.toggle('cba-index-open');
                } else {
                  page.classList.toggle('cba-index-collapsed');
                }
                syncIndexButtons();
                return;
              }

              var close = e.target.closest('[data-cba-index-close]');
              if (close) {
                closeCompactIndex();
                return;
              }

              var termCard = e.target.closest('[data-cba-term]');
              if (termCard) {
                Shiny.setInputValue(
                  '%s',
                  termCard.getAttribute('data-cba-term'),
                  {priority:'event'}
                );
                if (compactIndex()) {
                  closeCompactIndex();
                }
                return;
              }

              var related = e.target.closest('[data-cba-related]');
              if (related) {
                Shiny.setInputValue(
                  '%s',
                  related.getAttribute('data-cba-related'),
                  {priority:'event'}
                );
                return;
              }

              var history = e.target.closest('[data-cba-history]');
              if (history) {
                Shiny.setInputValue(
                  '%s',
                  history.getAttribute('data-cba-history'),
                  {priority:'event'}
                );
              }
            });

            page.addEventListener('keydown', function(e) {
              if (e.key === 'Escape' && page.classList.contains('cba-index-open')) {
                closeCompactIndex();
              }
            });

            syncIndexButtons();
          })();
          ",
          ns("root"),
          ns("selected_term"),
          ns("selected_related"),
          ns("selected_history")
        )
      )
    ),
    
    # --------------------------------------------------------
    # Hero
    # --------------------------------------------------------
    
    shiny::div(
      class = "cba-kb-hero",
      
      shiny::div(
        shiny::div(
          class = "cba-kb-eyebrow",
          "BASKETBALL OPERATIONS CBA REFERENCE"
        ),
        
        shiny::h2(
          class = "cba-kb-title",
          "CBA Info Hub"
        ),
        
        shiny::p(
          class = "cba-kb-subtitle",
          paste(
            "Search NBA roster-building terminology, understand the rule in plain language,",
            "and connect it directly to front-office decisions."
          )
        )
      ),
      
      shiny::div(
        class = "cba-kb-count",
        shiny::strong(
          shiny::textOutput(
            ns("term_count"),
            inline = TRUE
          )
        ),
        shiny::span("TERMS")
      )
    ),
    
    # --------------------------------------------------------
    # Main knowledge-base workspace
    # --------------------------------------------------------

    shiny::tags$button(
      type = "button",
      class = "cba-kb-mobile-index-toggle",
      `data-cba-index-toggle` = "true",
      `aria-expanded` = "false",
      `aria-controls` = ns("index_panel"),
      shiny::span("Knowledge Index"),
      shiny::span(
        `aria-hidden` = "true",
        "Open"
      )
    ),
    
    shiny::div(
      class = "cba-kb-layout",
      
      shiny::div(
        id = ns("index_panel"),
        class = "cba-kb-panel cba-kb-index-panel",
        
        shiny::div(
          class = "cba-kb-panel-head",
          shiny::strong("Knowledge Index"),
          shiny::span(
            shiny::textOutput(
              ns("result_count"),
              inline = TRUE
            )
          ),
          shiny::tags$button(
            type = "button",
            class = "cba-kb-index-toggle",
            `data-cba-index-toggle` = "true",
            `aria-expanded` = "true",
            `aria-controls` = ns("index_panel"),
            title = "Collapse or expand the CBA Knowledge Index",
            shiny::span(
              class = "cba-kb-index-toggle-icon",
              `aria-hidden` = "true",
              shiny::HTML("&lsaquo;")
            ),
            shiny::span(
              class = "cba-kb-index-toggle-label",
              "Collapse"
            )
          ),
          shiny::tags$button(
            type = "button",
            class = "cba-kb-index-close",
            `data-cba-index-close` = "true",
            "Close"
          )
        ),

        shiny::div(
          class = "cba-kb-search-panel",

          shiny::textInput(
            ns("search"),
            "Search terminology",
            placeholder = "Second apron, Bird rights, TPE, qualifying offer..."
          ),

          shiny::selectInput(
            ns("category"),
            "Category",
            choices = c(
              "All Categories",
              sort(unique(tbi_cba_glossary_data()$category))
            ),
            selected = "All Categories"
          )
        ),
        
        shiny::uiOutput(
          ns("glossary_list")
        )
      ),
      
      shiny::div(
        class = "cba-kb-panel cba-kb-workspace-panel",
        
        shiny::div(
          class = "cba-kb-panel-head",
          shiny::strong("Rule Intelligence"),
          shiny::span("CONTEXT + APPLICATION")
        ),
        
        shiny::uiOutput(
          ns("term_detail")
        )
      ),

      shiny::div(
        class = "cba-kb-drawer-scrim",
        `data-cba-index-close` = "true",
        `aria-hidden` = "true"
      )
    ),
    
    # --------------------------------------------------------
    # Favorites + recently viewed
    # --------------------------------------------------------
    
    shiny::div(
      class = "cba-kb-history",
      
      shiny::div(
        class = "cba-kb-panel cba-kb-mini-list",
        shiny::div(
          class = "cba-kb-mini-title",
          "RECENTLY VIEWED"
        ),
        shiny::uiOutput(
          ns("recent_terms")
        )
      ),
      
      shiny::div(
        class = "cba-kb-panel cba-kb-mini-list",
        shiny::div(
          class = "cba-kb-mini-title",
          "FAVORITES"
        ),
        shiny::uiOutput(
          ns("favorite_terms")
        )
      )
    ),
    
    shiny::div(
      class = "cba-kb-note",
      shiny::HTML(
        paste(
          "<strong>Decision-support notice:</strong>",
          "This knowledge base summarizes CBA concepts for basketball-operations use.",
          "Transaction legality, eligibility, dates, award criteria, and exception availability",
          "should still be verified against the governing CBA and official league guidance."
        )
      )
    )
  )
}


# ============================================================
# SERVER
# ============================================================

#' CBA Knowledge Base server
#'
#' @param id Internal module ID.
#' @noRd
mod_cba_glossary_server <- function(
    id,
    external_term = NULL) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      glossary <- tbi_cba_glossary_data()
      
      selected_term_value <- shiny::reactiveVal(
        glossary$term[[1]]
      )
      
      recent_values <- shiny::reactiveVal(
        character()
      )
      
      favorite_values <- shiny::reactiveVal(
        character()
      )
      
      # ------------------------------------------------------
      # Helpers
      # ------------------------------------------------------
      
      split_pipe <- function(x) {
        x <- as.character(x %||% "")
        
        if (
          !length(x) ||
          is.na(x[[1]]) ||
          !nzchar(
            trimws(
              x[[1]]
            )
          )
        ) {
          return(character())
        }
        
        values <- strsplit(
          x[[1]],
          "\\|"
        )[[1]]
        
        unique(
          trimws(
            values[
              nzchar(
                trimws(values)
              )
            ]
          )
        )
      }
      
      
      impacted_tbi_modules <- function(row) {
        
        category <- as.character(row$category[[1]])
        primary <- as.character(row$module[[1]])
        
        modules <- switch(
          category,
          "Aprons" = c(
            "Executive Dashboard",
            "Cap Intelligence",
            "Trade Intelligence",
            "Extension Simulator",
            "Five-Year Outlook"
          ),
          "Cap Structure" = c(
            "Executive Dashboard",
            "Cap Intelligence",
            "Trade Intelligence",
            "Extension Simulator",
            "Five-Year Outlook"
          ),
          "Trades" = c(
            "Executive Dashboard",
            "Cap Intelligence",
            "Trade Intelligence",
            "Player Management",
            "Five-Year Outlook"
          ),
          "Extensions" = c(
            "Executive Dashboard",
            "Player Management",
            "Cap Intelligence",
            "Extension Simulator",
            "Five-Year Outlook"
          ),
          "Free Agency" = c(
            "Executive Dashboard",
            "Player Management",
            "Cap Intelligence",
            "Extension Simulator",
            "Five-Year Outlook"
          ),
          "Exceptions" = c(
            "Executive Dashboard",
            "Cap Intelligence",
            "Trade Intelligence",
            "Player Management",
            "Five-Year Outlook"
          ),
          "Contracts" = c(
            "Roster Intelligence",
            "Player Management",
            "Cap Intelligence",
            "Extension Simulator",
            "Trade Intelligence",
            "Five-Year Outlook"
          ),
          "Draft Assets" = c(
            "Executive Dashboard",
            "Trade Intelligence",
            "Draft Intelligence",
            "Five-Year Outlook"
          ),
          "Transaction Restrictions" = c(
            "Executive Dashboard",
            "Cap Intelligence",
            "Trade Intelligence",
            "Five-Year Outlook"
          ),
          c(primary)
        )
        
        unique(c(primary, modules))
      }
      
      
      select_term <- function(term) {
        resolved_term <- tbi_cba_resolve_term(
          term,
          glossary = glossary
        )

        if (is.na(resolved_term)) {
          return(
            invisible(FALSE)
          )
        }

        term <- resolved_term
        
        selected_term_value(
          term
        )
        
        recent <- recent_values()
        
        recent <- c(
          term,
          recent[
            recent != term
          ]
        )
        
        recent_values(
          utils::head(
            recent,
            8
          )
        )
        
        invisible(TRUE)
      }
      
      # ------------------------------------------------------
      # External CBA deep-link requests
      # ------------------------------------------------------
      
      if (
        !is.null(external_term) &&
        is.function(external_term)
      ) {
        
        shiny::observeEvent(
          external_term(),
          {
            
            request <- external_term()
            
            if (is.null(request)) {
              return()
            }
            
            term <- if (
              is.list(request) &&
              "term" %in% names(request)
            ) {
              request$term
            } else {
              request
            }
            
            select_term(term)
          },
          ignoreInit = TRUE
        )
      }
      
      
      # ------------------------------------------------------
      # Filtering
      # ------------------------------------------------------
      
      filtered_glossary <- shiny::reactive({
        category <- input$category %||%
          "All Categories"

        tbi_cba_filter_glossary(
          glossary,
          search_value = input$search %||% "",
          category = category
        )
      })
      
      # ------------------------------------------------------
      # Selection handlers
      # ------------------------------------------------------
      
      shiny::observeEvent(
        input$selected_term,
        {
          select_term(
            input$selected_term
          )
        },
        ignoreInit = TRUE
      )
      
      shiny::observeEvent(
        input$selected_related,
        {
          select_term(
            input$selected_related
          )
        },
        ignoreInit = TRUE
      )
      
      shiny::observeEvent(
        input$selected_history,
        {
          select_term(
            input$selected_history
          )
        },
        ignoreInit = TRUE
      )
      
      shiny::observeEvent(
        input$toggle_favorite,
        {
          term <- selected_term_value()
          favorites <- favorite_values()
          
          if (
            term %in%
            favorites
          ) {
            favorites <- favorites[
              favorites != term
            ]
          } else {
            favorites <- c(
              term,
              favorites
            )
          }
          
          favorite_values(
            unique(
              favorites
            )
          )
        },
        ignoreInit = TRUE
      )
      
      # Filtering changes only the Knowledge Index. The detail selection is
      # owned by an explicit term click or deep-link request so an active
      # search cannot replace the requested rule.
      
      # Track initial term.
      shiny::observe({
        if (
          !length(
            recent_values()
          )
        ) {
          recent_values(
            selected_term_value()
          )
        }
      })
      
      # ------------------------------------------------------
      # Counts
      # ------------------------------------------------------
      
      output$term_count <- shiny::renderText({
        nrow(glossary)
      })
      
      output$result_count <- shiny::renderText({
        count <- nrow(
          filtered_glossary()
        )
        
        paste0(
          count,
          " RESULT",
          if (
            count == 1
          ) "" else "S"
        )
      })
      
      # ------------------------------------------------------
      # Index
      # ------------------------------------------------------
      
      output$glossary_list <- shiny::renderUI({
        
        d <- filtered_glossary()
        
        if (!nrow(d)) {
          return(
            shiny::div(
              class = "cba-kb-empty",
              "No CBA terms match the current search."
            )
          )
        }
        
        category_groups <- lapply(
          unique(d$category),
          function(category_name) {
            category_rows <- which(d$category == category_name)

            shiny::tags$section(
              class = "cba-kb-index-group",
              `aria-label` = category_name,
              shiny::h3(
                class = "cba-kb-index-group-title",
                category_name
              ),
              lapply(
                category_rows,
                function(i) {
                  row <- d[i, , drop = FALSE]
                  selected <- identical(
                    row$term[[1]],
                    selected_term_value()
                  )

                  shiny::div(
                    class = paste(
                      "cba-kb-term-card",
                      if (selected) "selected" else ""
                    ),
                    `data-cba-term` = row$term[[1]],

                    shiny::div(
                      class = "cba-kb-term-top",

                      shiny::span(
                        class = "cba-kb-term-name",
                        row$term[[1]]
                      ),

                      shiny::span(
                        class = "cba-kb-term-category",
                        row$category[[1]]
                      )
                    ),

                    shiny::div(
                      class = "cba-kb-preview",
                      row$short_definition[[1]]
                    )
                  )
                }
              )
            )
          }
        )
        
        shiny::div(
          class = "cba-kb-list",
          category_groups
        )
      })
      
      # ------------------------------------------------------
      # Detail panel
      # ------------------------------------------------------
      
      output$term_detail <- shiny::renderUI({
        
        term <- selected_term_value()
        
        row <- glossary[
          glossary$term == term,
          ,
          drop = FALSE
        ]
        
        if (!nrow(row)) {
          return(
            shiny::div(
              class = "cba-kb-empty",
              "Select a knowledge-base term."
            )
          )
        }
        
        related <- split_pipe(
          row$related_terms
        )
        
        related <- related[
          related %in%
            glossary$term
        ]
        
        affects <- split_pipe(
          row$affects
        )

        aliases <- split_pipe(
          row$aliases
        )
        
        favorite <- term %in%
          favorite_values()

        active_modules <- impacted_tbi_modules(row)
        
        shiny::div(
          class = "cba-kb-detail-body",
          
          shiny::div(
            class = "cba-kb-category",
            `data-cba-detail-block` = "category",
            row$category[[1]]
          ),
          
          shiny::div(
            class = "cba-kb-term-title-row",
            `data-cba-detail-block` = "title",
            
            shiny::div(
              shiny::div(
                class = "cba-kb-term-title",
                row$term[[1]]
              ),
              
              shiny::div(
                class = "cba-kb-module",
                paste0(
                  "Primary TBI module: ",
                  row$module[[1]]
                )
              )
            ),
            
            shiny::actionButton(
              session$ns(
                "toggle_favorite"
              ),
              shiny::HTML(
                if (favorite) {
                  "&#9733; FAVORITE"
                } else {
                  "&#9734; FAVORITE"
                }
              ),
              class = paste(
                "cba-kb-favorite-btn",
                if (
                  favorite
                ) "active" else ""
              )
            )
          ),
          
          shiny::div(
            class = "cba-kb-section",
            `data-cba-detail-block` = "meaning",
            
            shiny::div(
              class = "cba-kb-label",
              "WHAT IT MEANS"
            ),
            
            shiny::p(
              class = "cba-kb-text",
              row$short_definition[[1]]
            )
          ),
          
          shiny::div(
            class = "cba-kb-impact",
            `data-cba-detail-block` = "impact",
            
            shiny::div(
              class = "cba-kb-label",
              "WHY A FRONT OFFICE CARES"
            ),
            
            shiny::p(
              class = "cba-kb-text",
              row$front_office_impact[[1]]
            )
          ),
          
          shiny::div(
            class = "cba-kb-example",
            `data-cba-detail-block` = "example",
            
            shiny::div(
              class = "cba-kb-label",
              "TRANSACTION EXAMPLE"
            ),
            
            shiny::p(
              class = "cba-kb-text",
              row$example[[1]]
            )
          ),
          
          shiny::div(
            class = "cba-kb-section",
            `data-cba-detail-block` = "affects",
            
            shiny::div(
              class = "cba-kb-label",
              "AFFECTS"
            ),
            
            shiny::div(
              class = "cba-kb-chip-wrap",
              lapply(
                affects,
                function(value) {
                  shiny::span(
                    class = "cba-kb-chip",
                    value
                  )
                }
              )
            )
          ),
          
          shiny::div(
            class = "cba-kb-section",
            `data-cba-detail-block` = "related",
            
            shiny::div(
              class = "cba-kb-label",
              "RELATED RULES"
            ),
            
            if (
              length(related)
            ) {
              shiny::div(
                class = "cba-kb-chip-wrap",
                lapply(
                  related,
                  function(value) {
                    shiny::span(
                      class = "cba-kb-chip cba-kb-related",
                      `data-cba-related` =
                        value,
                      value
                    )
                  }
                )
              )
            } else {
              shiny::div(
                class = "cba-kb-mini-empty",
                "No related rules are mapped yet."
              )
            },

            if (length(aliases)) {
              shiny::tagList(
                shiny::div(
                  class = "cba-kb-label cba-kb-alias-label",
                  "ALIASES + COMMON SHORTHAND"
                ),
                shiny::div(
                  class = "cba-kb-chip-wrap",
                  lapply(
                    aliases,
                    function(value) {
                      shiny::span(
                        class = "cba-kb-chip",
                        value
                      )
                    }
                  )
                )
              )
            } else {
              NULL
            }
          ),
          
          shiny::div(
            class = "cba-kb-section",
            `data-cba-detail-block` = "used",
            
            shiny::div(
              class = "cba-kb-label",
              "USED IN TBI"
            ),
            
            shiny::p(
              class = "cba-kb-text",
              row$module[[1]]
            )
          ),
          
          shiny::div(
            class = "cba-kb-rule-impact",
            `data-cba-detail-block` = "rule-impact",
            
            shiny::div(
              class = "cba-kb-label",
              "RULE IMPACT ACROSS TBI"
            ),
            
            shiny::div(
              class = "cba-kb-impact-grid",
              
              lapply(
                c(
                  "Executive Dashboard",
                  "Roster Intelligence",
                  "Player Management",
                  "Cap Intelligence",
                  "Trade Intelligence",
                  "Extension Simulator",
                  "Draft Intelligence",
                  "Five-Year Outlook"
                ),
                function(module_name) {
                  active <- module_name %in% active_modules
                  
                  shiny::div(
                    class = paste(
                      "cba-kb-impact-module clickable",
                      if (active) "active" else ""
                    ),
                    `data-cba-module-link` = module_name,
                    
                    shiny::span(
                      class = "cba-kb-impact-check",
                      shiny::HTML(
                        if (active) "&#10003;" else "&#183;"
                      )
                    ),
                    
                    shiny::span(
                      module_name
                    )
                  )
                }
              )
            )
          ),
          
          shiny::div(
            class = "cba-kb-source",
            `data-cba-detail-block` = "source",
            shiny::div(
              shiny::strong("Source framework: "),
              row$source[[1]]
            ),
            shiny::div(
              shiny::strong("Rule reference: "),
              row$source_reference[[1]]
            ),
            shiny::div(
              class = paste(
                "cba-kb-source-status",
                if (identical(
                  row$verification_status[[1]],
                  "Requires source verification"
                )) {
                  "requires-verification"
                } else {
                  ""
                }
              ),
              row$verification_status[[1]]
            ),
            shiny::div(
              class = "cba-kb-source-note",
              paste(
                "Content is summarized for basketball-operations decision support.",
                "Confirm the governing agreement and official league guidance before execution."
              )
            )
          )
        )
      })
      
      # ------------------------------------------------------
      # Recently viewed
      # ------------------------------------------------------
      
      output$recent_terms <- shiny::renderUI({
        
        values <- recent_values()
        
        if (!length(values)) {
          return(
            shiny::div(
              class = "cba-kb-mini-empty",
              "No terms viewed yet."
            )
          )
        }
        
        shiny::tagList(
          lapply(
            values,
            function(value) {
              shiny::div(
                class = "cba-kb-mini-link",
                `data-cba-history` =
                  value,
                value
              )
            }
          )
        )
      })
      
      # ------------------------------------------------------
      # Favorites
      # ------------------------------------------------------
      
      output$favorite_terms <- shiny::renderUI({
        
        values <- favorite_values()
        
        if (!length(values)) {
          return(
            shiny::div(
              class = "cba-kb-mini-empty",
              "Star a rule to keep it here."
            )
          )
        }
        
        shiny::tagList(
          lapply(
            values,
            function(value) {
              shiny::div(
                class = "cba-kb-mini-link",
                `data-cba-history` =
                  value,
                value
              )
            }
          )
        )
      })
    }
  )
}
