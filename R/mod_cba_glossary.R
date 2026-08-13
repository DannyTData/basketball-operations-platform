# ------------------------------------------------------------
# Module: CBA Knowledge Base
# Thompson Basketball Intelligence
# Version 2
# ------------------------------------------------------------

# ============================================================
# DATA
# ============================================================

#' CBA knowledge-base data
#' @noRd
tbi_cba_glossary_data <- function() {
  
  data.frame(
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
          padding:12px 14px;
          display:grid;
          grid-template-columns:minmax(0,1.4fr) minmax(180px,.55fr);
          gap:10px;
          border:1px solid rgba(148,163,184,.10);
          border-radius:12px;
          background:rgba(15,25,40,.82);
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
          grid-template-columns:minmax(0,1.05fr) minmax(360px,.95fr);
          gap:12px;
          align-items:start;
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
          max-height:640px;
          padding:9px;
          overflow:auto;
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
        }

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

        .cba-kb-text {
          margin:0;
          color:#c4ceda;
          font-size:.64rem;
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

        @media(max-width:1000px) {
          .cba-kb-layout {
            grid-template-columns:1fr;
          }

          .cba-kb-list {
            max-height:420px;
          }
        }

        @media(max-width:650px) {
          .cba-kb-hero,
          .cba-kb-search-panel,
          .cba-kb-history {
            grid-template-columns:1fr;
          }
        }
        "
      )
    ),
    
    shiny::tags$script(
      shiny::HTML(
        sprintf(
          "
          document.addEventListener('click', function(e) {

            var termCard = e.target.closest('[data-cba-term]');
            if (termCard) {
              Shiny.setInputValue(
                '%s',
                termCard.getAttribute('data-cba-term'),
                {priority:'event'}
              );
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
              return;
            }
          });
          ",
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
    # Search + category
    # --------------------------------------------------------
    
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
          "Aprons",
          "Cap Structure",
          "Contracts",
          "Exceptions",
          "Extensions",
          "Free Agency",
          "Trades"
        ),
        selected = "All Categories"
      )
    ),
    
    # --------------------------------------------------------
    # Main knowledge-base workspace
    # --------------------------------------------------------
    
    shiny::div(
      class = "cba-kb-layout",
      
      shiny::div(
        class = "cba-kb-panel",
        
        shiny::div(
          class = "cba-kb-panel-head",
          shiny::strong("Knowledge Index"),
          shiny::span(
            shiny::textOutput(
              ns("result_count"),
              inline = TRUE
            )
          )
        ),
        
        shiny::uiOutput(
          ns("glossary_list")
        )
      ),
      
      shiny::div(
        class = "cba-kb-panel",
        
        shiny::div(
          class = "cba-kb-panel-head",
          shiny::strong("Rule Intelligence"),
          shiny::span("CONTEXT + APPLICATION")
        ),
        
        shiny::uiOutput(
          ns("term_detail")
        )
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
          c(primary)
        )
        
        unique(c(primary, modules))
      }
      
      
      select_term <- function(term) {
        
        term <- as.character(
          term %||% ""
        )
        
        if (
          !length(term) ||
          !term[[1]] %in%
          glossary$term
        ) {
          return(
            invisible(FALSE)
          )
        }
        
        term <- term[[1]]
        
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
        
        d <- glossary
        
        category <- input$category %||%
          "All Categories"
        
        search_value <- trimws(
          tolower(
            input$search %||%
              ""
          )
        )
        
        if (
          !identical(
            category,
            "All Categories"
          )
        ) {
          d <- d[
            d$category == category,
            ,
            drop = FALSE
          ]
        }
        
        if (nzchar(search_value)) {
          
          haystack <- tolower(
            paste(
              d$term,
              d$category,
              d$short_definition,
              d$front_office_impact,
              d$example,
              d$affects,
              d$related_terms,
              d$module
            )
          )
          
          keep <- grepl(
            search_value,
            haystack,
            fixed = TRUE
          )
          
          d <- d[
            keep,
            ,
            drop = FALSE
          ]
        }
        
        d
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
      
      shiny::observeEvent(
        filtered_glossary(),
        {
          d <- filtered_glossary()
          
          if (
            nrow(d) &&
            !selected_term_value() %in%
            d$term
          ) {
            select_term(
              d$term[[1]]
            )
          }
        },
        ignoreInit = TRUE
      )
      
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
        
        cards <- lapply(
          seq_len(
            nrow(d)
          ),
          function(i) {
            
            row <- d[
              i,
              ,
              drop = FALSE
            ]
            
            selected <- identical(
              row$term[[1]],
              selected_term_value()
            )
            
            shiny::div(
              class = paste(
                "cba-kb-term-card",
                if (
                  selected
                ) "selected" else ""
              ),
              `data-cba-term` =
                row$term[[1]],
              
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
        
        shiny::div(
          class = "cba-kb-list",
          cards
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
        
        favorite <- term %in%
          favorite_values()
        
        shiny::div(
          class = "cba-kb-detail-body",
          
          shiny::div(
            class = "cba-kb-category",
            row$category[[1]]
          ),
          
          shiny::div(
            class = "cba-kb-term-title-row",
            
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
              if (
                favorite
              ) {
                "★ FAVORITE"
              } else {
                "☆ FAVORITE"
              },
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
            }
          ),
          
          shiny::div(
            class = "cba-kb-section",
            
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
                  
                  active_modules <- impacted_tbi_modules(row)
                  active <- module_name %in% active_modules
                  
                  shiny::div(
                    class = paste(
                      "cba-kb-impact-module clickable",
                      if (active) "active" else ""
                    ),
                    `data-cba-module-link` = module_name,
                    
                    shiny::span(
                      class = "cba-kb-impact-check",
                      if (active) "✓" else "·"
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
            shiny::HTML(
              paste0(
                "<strong>Source framework:</strong> ",
                row$source[[1]],
                ". Content is summarized for basketball-operations decision support."
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