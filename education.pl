:- use_module(library(lists)).

:- table content_covers_skill/2.

% --- Discontiguous declarations ---
:- discontiguous subclass_of/2.
:- discontiguous entity/3.
:- discontiguous rel/4.
:- discontiguous user_info/2.
:- discontiguous container_info/2.
:- discontiguous assessment_tool_info/2.
:- discontiguous category_info/2. % For skills and topics
:- discontiguous event_info/2.
:- discontiguous accomplishment_info/2.
:- discontiguous project_info/2.
:- discontiguous is_subclass_normal/2.

% --- SCHEMA: Entity Hierarchy ---
% Using 'DomainEntity' as the root class name
subclass_of('User', 'DomainEntity').
subclass_of('Container', 'DomainEntity').
subclass_of('AssessmentTool', 'DomainEntity').
subclass_of('Category', 'DomainEntity'). % Broad category for Skills, Topics
subclass_of('Event', 'DomainEntity').
subclass_of('Accomplishment', 'DomainEntity').
subclass_of('Project', 'DomainEntity').

% Subclasses of Container
subclass_of('Course', 'Container').
subclass_of('Module', 'Container').
subclass_of('Lesson', 'Container').
subclass_of('Workshop', 'Container').
subclass_of('AssessmentContainer', 'Container'). % A container that is also an assessment

% Subclasses of Category (more specific, if needed, or keep general)
subclass_of('Skill', 'Category').
subclass_of('Topic', 'Category').

% --- SCHEMA: Relationships ---
% inverse_of(RelationName, InverseRelationName).
inverse_of(partOf, hasPart).
inverse_of(instanceOf, hasInstance).
inverse_of(involvedInEvent, eventHasParticipant).
inverse_of(hasSkill, skillOfUser). % User hasSkill Skill vs Skill skillOfUser User
inverse_of(assessesSkill, skillAssessedBy).
inverse_of(evolvesTo, evolvesFrom).
inverse_of(requires, requiredBy).
inverse_of(ownsAccomplishment, accomplishmentOwnedBy).
inverse_of(awardedFor, awardsAccomplishment).
inverse_of(developed, developedBy). % User developed Content vs Content developedBy User
inverse_of(interestedInProject, projectHasInterestedUser).
inverse_of(rated, ratingOf).

% 'RelationName'([DomainClass-RangeClass, ...], [attributeOfRelationship1, ...]).
'partOf'(['Container'-'Container', 'AssessmentContainer'-'Container', 'Lesson'-'Module', 'Module'-'Course'], []). % Component-Parent
'hasPart'(['Container'-'Container', 'Container'-'AssessmentContainer', 'Module'-'Lesson', 'Course'-'Module'], []). % Parent-Component

'instanceOf'(['Event'-'Container'], []). % Event is instanceOf Container
'hasInstance'(['Container'-'Event'], []). % Container hasInstance Event

'involvedInEvent'(['User'-'Event'], [role, startDate, endDate]). % User involvedInEvent Event
'eventHasParticipant'(['Event'-'User'], [role, startDate, endDate]). % Event eventHasParticipant User

'hasSkill'(['User'-'Category'], [level]). % User hasSkill Category (Skill/Topic)
'skillOfUser'(['Category'-'User'], [level]). % Category skillOfUser User

'assessesSkill'(['Container'-'Category'], [scale]). % AssessmentContainer assessesSkill Category
'skillAssessedBy'(['Category'-'Container'], [scale]). % Category skillAssessedBy AssessmentContainer

'evolvesTo'(['AssessmentTool'-'AssessmentTool', 'Container'-'Container'], []). % Old evolvesTo New
'evolvesFrom'(['AssessmentTool'-'AssessmentTool', 'Container'-'Container'], []). % New evolvesFrom Old

'requires'(['Container'-'Container', 'Container'-'AssessmentTool'], []). % Container requires Prerequisite
'requiredBy'(['Container'-'Container', 'AssessmentTool'-'Container'], []). % Prerequisite requiredBy Container

'ownsAccomplishment'(['User'-'Accomplishment'], [score, issueDate, expiryDate, isValid]).
'accomplishmentOwnedBy'(['Accomplishment'-'User'], [score, issueDate, expiryDate, isValid]).

'awardedFor'(['Accomplishment'-'Container'], []).
'awardsAccomplishment'(['Container'-'Accomplishment'], []).

'developed'(['User'-'Container', 'User'-'AssessmentTool'], [role, date, version]). % User developed Content
'developedBy'(['Container'-'User', 'AssessmentTool'-'User'], [role, date, version]). % Content developedBy User

'interestedInProject'(['User'-'Project'], []).
'projectHasInterestedUser'(['Project'-'User'], []).

'rated'(['User'-'Container'], [ratingValue]). % User rated Content
'ratingOf'(['Container'-'User'], [ratingValue]). % Content ratingOf User


% --- FACTS: Specific Info Predicates (for core identification) ---
user_info(u1, 'Alice').
user_info(u2, 'Bob_Teacher').
user_info(u3, 'Charlie_Learner').
user_info(u4, 'David_Mentor').
user_info(u5, 'Eve_Student').
user_info(u6, 'Fiona_Expert').
user_info(u7, 'Charles_Teacher').

container_info(cos101, 'AI Course').
container_info(mod_cpp, 'C++ Module').
container_info(mod_prolog, 'Prolog Module').
container_info(mod_search, 'Search Algos Module').
container_info(les_recursion, 'Recursion Lesson').
container_info(asm_prolog_quiz, 'Prolog Quiz').
container_info(workshop_advanced_prolog, 'Advanced Prolog Workshop').
container_info(mod_prolog_2005, 'Prolog Lesson 2005/06').

assessment_tool_info(qn1, 'Prolog Concepts QNR').
assessment_tool_info(qn2, 'Search Algo QNR').
assessment_tool_info(qn2_old, 'Search Algo QNR Old').

category_info(skill_prolog, 'Prolog Programming').
category_info(skill_search, 'Search Algorithms').
category_info(skill_recursion, 'Recursion').
category_info(skill_expert_prolog, 'Expert Prolog Techniques').
category_info(topic_ai_ethics, 'AI Ethics').

event_info(evt_prolog_les, 'Prolog Lesson Event').
event_info(evt_search_mod, 'Search Module Event').
event_info(evt_ai_course_active, 'AI Course Active Instance').
event_info(evt_adv_prolog_ws, 'Advanced Prolog Workshop Event').

accomplishment_info(ac_prolog_module_cert, 'Prolog Module Certificate').
accomplishment_info(ac_search_module_pass, 'Search Module Pass').
accomplishment_info(ac_expert_prolog_badge, 'Expert Prolog Badge').

project_info(p1, 'AI Chatbot').
project_info(p2, 'Game Pathfinding').

% --- FACTS: Generic Entity Facts ---
% entity(EntityID, EntityType, [ListOfAttributes])
entity(u1, 'User', [attr(name, 'Alice')]).
entity(u2, 'User', [attr(name, 'Bob_Teacher')]).
entity(u3, 'User', [attr(name, 'Charlie_Learner')]).
entity(u4, 'User', [attr(name, 'David_Mentor')]).
entity(u5, 'User', [attr(name, 'Eve_Student')]).
entity(u6, 'User', [attr(name, 'Fiona_Expert')]).
entity(u7, 'User', [attr(name, 'Charles_Teacher')]).

entity(cos101, 'Course', [attr(name, 'AI Course'), attr(language, lang_en)]).
entity(mod_cpp, 'Module', [attr(name, 'C++ Module'), attr(language, lang_en), attr(difficulty, unrated)]).
entity(mod_prolog, 'Module', [attr(name, 'Prolog Module'), attr(language, lang_en), attr(difficulty, beginner)]).
entity(mod_search, 'Module', [attr(name, 'Search Algos Module'), attr(language, lang_en), attr(difficulty, intermediate)]).
entity(les_recursion, 'Lesson', [attr(name, 'Recursion Lesson'), attr(language, lang_en), attr(difficulty, beginner)]).
entity(asm_prolog_quiz, 'AssessmentContainer', [attr(name, 'Prolog Quiz'), attr(language, lang_en)]).
entity(workshop_advanced_prolog, 'Workshop', [attr(name, 'Advanced Prolog Workshop'), attr(language, lang_en), attr(difficulty, advanced)]).
entity(mod_prolog_2005, 'Module', [attr(name, 'Prolog Lesson 2005/06'), attr(language, lang_en)]).

entity(qn1, 'AssessmentTool', [attr(name, 'Prolog Concepts QNR'), attr(language, lang_en), attr(version, 'v1')]).
entity(qn2, 'AssessmentTool', [attr(name, 'Search Algo QNR'), attr(language, lang_en), attr(version, 'v2')]).
entity(qn2_old, 'AssessmentTool', [attr(name, 'Search Algo QNR Old'), attr(language, lang_en), attr(version, 'v1')]).

entity(skill_prolog, 'Skill', [attr(name, 'Prolog Programming')]).
entity(skill_search, 'Skill', [attr(name, 'Search Algorithms')]).
entity(skill_recursion, 'Skill', [attr(name, 'Recursion')]).
entity(skill_expert_prolog, 'Skill', [attr(name, 'Expert Prolog Techniques')]).
entity(topic_ai_ethics, 'Topic', [attr(name, 'AI Ethics')]).

entity(evt_prolog_les, 'Event', [attr(name, 'Prolog Lesson Event')]).
entity(evt_search_mod, 'Event', [attr(name, 'Search Module Event')]).
entity(evt_ai_course_active, 'Event', [attr(name, 'AI Course Active Instance')]).
entity(evt_adv_prolog_ws, 'Event', [attr(name, 'Advanced Prolog Workshop Event')]).

entity(ac_prolog_module_cert, 'Accomplishment', [attr(name, 'Prolog Module Certificate')]).
entity(ac_search_module_pass, 'Accomplishment', [attr(name, 'Search Module Pass')]).
entity(ac_expert_prolog_badge, 'Accomplishment', [attr(name, 'Expert Prolog Badge')]).

entity(p1, 'Project', [attr(name, 'AI Chatbot'), attr(requiredSkill, skill_prolog)]).
entity(p2, 'Project', [attr(name, 'Game Pathfinding'), attr(requiredSkill, skill_search)]).


% --- FACTS: Generic Relationship Facts ---
% rel(RelationName, SubjectID, ObjectID, [ListOfAttributes])
rel(partOf, mod_prolog, cos101, []).
rel(partOf, mod_search, cos101, []).
rel(partOf, les_recursion, mod_prolog, []).
rel(partOf, asm_prolog_quiz, mod_prolog, []).
rel(partOf, workshop_advanced_prolog, cos101, []).
rel(partOf, mod_cpp, cos101, []).

rel(instanceOf, evt_prolog_les, les_recursion, []).
rel(instanceOf, evt_search_mod, mod_search, []).
rel(instanceOf, evt_ai_course_active, cos101, []).
rel(instanceOf, evt_adv_prolog_ws, workshop_advanced_prolog, []).

rel(involvedInEvent, u3, evt_prolog_les, [attr(role, 'Learner')]).
rel(involvedInEvent, u5, evt_prolog_les, [attr(role, 'Learner')]).
rel(involvedInEvent, u4, evt_prolog_les, [attr(role, 'Teacher')]).
rel(involvedInEvent, u3, evt_search_mod, [attr(role, 'Learner')]).
rel(involvedInEvent, u3, evt_adv_prolog_ws, [attr(role, 'Learner')]).
rel(involvedInEvent, u6, evt_adv_prolog_ws, [attr(role, 'Speaker')]).
rel(involvedInEvent, u4, evt_adv_prolog_ws, [attr(role, 'Learner')]).
rel(involvedInEvent, u5, evt_adv_prolog_ws, [attr(role, 'Learner')]).

rel(hasSkill, u3, skill_prolog, [attr(level, 6)]).
rel(hasSkill, u3, skill_search, [attr(level, 5)]).
rel(hasSkill, u3, skill_recursion, [attr(level, 5)]).
rel(hasSkill, u4, skill_prolog, [attr(level, 9)]).
rel(hasSkill, u4, skill_recursion, [attr(level, 8)]).
rel(hasSkill, u4, skill_expert_prolog, [attr(level, 7)]).
rel(hasSkill, u5, skill_prolog, [attr(level, 3)]).
rel(hasSkill, u6, skill_expert_prolog, [attr(level, 10)]).
rel(hasSkill, u2, topic_ai_ethics, [attr(level, 8)]).
rel(hasSkill, u7, skill_search, [attr(level, 8)]).

rel(assessesSkill, asm_prolog_quiz, skill_prolog, []).
rel(assessesSkill, asm_prolog_quiz, skill_recursion, []).
rel(assessesSkill, workshop_advanced_prolog, skill_expert_prolog, []). % Workshop is an AssessmentContainer here
rel(assessesSkill, mod_search, skill_search, [attr(scale, 'pass_fail_scale')]). % Module itself assesses

rel(evolvesTo, qn2_old, qn2, []). % qn2_old evolvesTo qn2

rel(requires, mod_search, qn2_old, []). % mod_search requires assessment_tool qn2_old
rel(requires, cos101, mod_prolog, []).
rel(requires, workshop_advanced_prolog, mod_prolog, []).

rel(ownsAccomplishment, u3, ac_prolog_module_cert, [attr(score, 85), attr(issueDate, '2023-01-01'), attr(expiryDate, '2023-02-28'), attr(isValid, true)]).
rel(ownsAccomplishment, u4, ac_prolog_module_cert, [attr(score, 95), attr(issueDate, '2022-01-01'), attr(expiryDate, '2022-02-28'), attr(isValid, true)]).
rel(ownsAccomplishment, u6, ac_expert_prolog_badge, [attr(issueDate, '2023-05-01'), attr(isValid, true)]). % score, expiryDate can be absent
rel(ownsAccomplishment, u7, ac_search_module_pass, [attr(issueDate, '2024-05-10'), attr(isValid, true)]).

rel(awardedFor, ac_prolog_module_cert, mod_prolog, []).
rel(awardedFor, ac_search_module_pass, mod_search, []).
rel(awardedFor, ac_expert_prolog_badge, workshop_advanced_prolog, []).

rel(developed, u4, mod_prolog, [attr(role, 'Creator')]).
rel(developed, u6, workshop_advanced_prolog, [attr(role, 'Creator')]).
rel(developed, u2, cos101, [attr(role, 'InstructionalDesigner')]).
rel(developed, u7, mod_search, [attr(role, 'Creator')]).

rel(interestedInProject, u3, p1, []).
rel(interestedInProject, u5, p1, []).
rel(interestedInProject, u4, p2, []).

rel(rated, u3, mod_prolog, [attr(ratingValue, 5)]).
rel(rated, u5, mod_prolog, [attr(ratingValue, 4)]).
rel(rated, u4, workshop_advanced_prolog, [attr(ratingValue, 5)]).


% --- GENERIC HELPER RULES ---
is_subclass(Subclass, Superclass) :-
  is_subclass_normal(Subclass, Superclass).

% Single, corrected definition of is_subclass_normal/2 (reflexive)
is_subclass_normal(Class, Class).
is_subclass_normal(Subclass, Superclass) :-
  subclass_of(Subclass, Superclass).
is_subclass_normal(Subclass, Superclass) :-
  subclass_of(Subclass, Middleclass),
  is_subclass_normal(Middleclass, Superclass).

get_attribute_value(EntityID, AttrName, Value) :-
    entity(EntityID, _, Attributes),
    member(attr(AttrName, Value), Attributes).
get_attribute_value(EntityID, AttrName, default) :-
    entity(EntityID, _, _),
    \+ (entity(EntityID, _, Attributes), member(attr(AttrName, _), Attributes)).

% Helper: transitively_part_of(ComponentID, ContainerID)
transitively_part_of(Component, Container) :- rel(partOf, Component, Container, _).
transitively_part_of(Component, Container) :-
    rel(partOf, Component, Intermediate, _),
    transitively_part_of(Intermediate, Container).

% --- content_covers_skill and ccs_check ---
content_covers_skill(ContainerID, SkillID) :-
    entity(ContainerID, ContainerType, _),
    is_subclass(ContainerType, 'Container'),
    entity(SkillID, SkillCatType, _),
    is_subclass(SkillCatType, 'Category'),
    ccs_check(ContainerID, SkillID, ContainerType).

ccs_check(ContainerID, SkillID, _ContainerType) :- % Rule CCS1
    rel(assessesSkill, ContainerID, SkillID, _).

ccs_check(ContainerID, SkillID, _ContainerType) :- % Rule CCS2
    rel(partOf, AssessmentPartID, ContainerID, _),
    entity(AssessmentPartID, PartType, _),
    is_subclass(PartType, 'AssessmentContainer'),
    rel(assessesSkill, AssessmentPartID, SkillID, _).

ccs_check(ContainerID, SkillID, 'Lesson') :- % Rule CCS3
    rel(partOf, ContainerID, ParentModuleID, _),
    entity(ParentModuleID, 'Module', _),
    content_covers_skill(ParentModuleID, SkillID).

ccs_check(ContainerID, SkillID, ContainerType) :- % Rule CCS4
    ContainerType \= 'Lesson',
    transitively_part_of(SubPartID, ContainerID),
    \+ (SubPartID == ContainerID), % Using \+ (==) instead of \@=
    content_covers_skill(SubPartID, SkillID).

% Helper: learner_mastered_container(UserID, ContainerID)
learner_mastered_container(UserID, ContainerID) :-
    entity(UserID, 'User', _),
    entity(ContainerID, ContainerType, _), is_subclass(ContainerType, 'Container'),
    rel(ownsAccomplishment, UserID, AccID, Attributes),
    member(attr(isValid, true), Attributes),
    entity(AccID, 'Accomplishment', _),
    rel(awardedFor, AccID, ContainerID, _).

% RULE 1: Peer Support Opportunity
peer_support_opportunity(SkilledLearner, LearnerToSupport, ContainerID, SkillID) :-
    entity(SkilledLearner, 'User', _), entity(LearnerToSupport, 'User', _),
    SkilledLearner \= LearnerToSupport,
    entity(ContainerID, ContainerType, _), is_subclass(ContainerType, 'Container'),
    entity(SkillID, SkillCatType, _), is_subclass(SkillCatType, 'Category'),
    rel(involvedInEvent, SkilledLearner, EventID, [attr(role, 'Learner')]),
    rel(involvedInEvent, LearnerToSupport, EventID, [attr(role, 'Learner')]),
    rel(instanceOf, EventID, ContainerID, _),
    content_covers_skill(ContainerID, SkillID),
    rel(hasSkill, SkilledLearner, SkillID, [attr(level, SkilledLevel)]),
    (   rel(hasSkill, LearnerToSupport, SkillID, [attr(level, SupportLevel)]),
        SkilledLevel > SupportLevel
    ;   \+ rel(hasSkill, LearnerToSupport, SkillID, _)
    ).

% RULE 2: Risk of Using Superseded Content
risk_of_superseded_content_use(OldContent, NewContent, active_event(EventID)) :-
    (entity(OldContent, OldType, _), (is_subclass(OldType, 'Container'); is_subclass(OldType, 'AssessmentTool'))),
    (entity(NewContent, NewType, _), (is_subclass(NewType, 'Container'); is_subclass(NewType, 'AssessmentTool'))),
    rel(evolvesTo, OldContent, NewContent, []),
    rel(instanceOf, EventID, OldContent, _),
    entity(EventID, 'Event', _).

risk_of_superseded_content_use(OldContent, NewContent, required_by_active_container(RequiringContainer)) :-
    (entity(OldContent, OldType, _), (is_subclass(OldType, 'Container'); is_subclass(OldType, 'AssessmentTool'))),
    (entity(NewContent, NewType, _), (is_subclass(NewType, 'Container'); is_subclass(NewType, 'AssessmentTool'))),
    rel(evolvesTo, OldContent, NewContent, []),
    rel(requires, RequiringContainer, OldContent, []),
    entity(RequiringContainer, ReqType, _), is_subclass(ReqType, 'Container'),
    rel(instanceOf, _, RequiringContainer, _).

% RULE 3: Course of Study Completion Status
get_all_mandatory_modules(CourseID, ModuleIDs) :-
    entity(CourseID, 'Course', _),
    findall(ModuleID,
            (   rel(partOf, ModuleID, CourseID, _),
                entity(ModuleID, 'Module', _)
            ),
            UnsortedModuleIDs),
    sort(UnsortedModuleIDs, ModuleIDs).

user_mastered_all_modules(_User, []).
user_mastered_all_modules(User, [ModuleH | ModuleT]) :-
    learner_mastered_container(User, ModuleH),
    user_mastered_all_modules(User, ModuleT).

user_completed_course_of_study(UserID, CourseID) :-
    entity(UserID, 'User', _),
    entity(CourseID, 'Course', _),
    get_all_mandatory_modules(CourseID, MandatoryModules),
    MandatoryModules \= [],
    user_mastered_all_modules(UserID, MandatoryModules).

% RULE 4: Overqualified Instructor for Introductory Content
overqualified_instructor_for_lesson(TeacherID, LessonID, SkillID) :-
    entity(TeacherID, 'User', _),
    entity(LessonID, 'Lesson', _),
    entity(SkillID, SkillCatType, _), is_subclass(SkillCatType, 'Category'),
    HighSkillThreshold = 8,
    rel(involvedInEvent, TeacherID, EventID, [attr(role, 'Teacher')]),
    rel(instanceOf, EventID, LessonID, _),
    rel(hasSkill, TeacherID, SkillID, [attr(level, TeacherSkillLevel)]),
    TeacherSkillLevel >= HighSkillThreshold,
    content_covers_skill(LessonID, SkillID),
    \+ rel(partOf, _, LessonID, _),
    rel(partOf, LessonID, _, _).

% RULE 5: Learner Lacks Prerequisite for Enrolled Event/Container
learner_lacks_prerequisite(UserID, CurrentContainerID, PrerequisiteContainerID) :-
    entity(UserID, 'User', _),
    rel(involvedInEvent, UserID, EventID, [attr(role, 'Learner')]),
    rel(instanceOf, EventID, CurrentContainerID, _),
    entity(CurrentContainerID, CurrContType, _), is_subclass(CurrContType, 'Container'),
    rel(requires, CurrentContainerID, PrerequisiteContainerID, []),
    entity(PrerequisiteContainerID, PrereqType, _), is_subclass(PrereqType, 'Container'),
    \+ learner_mastered_container(UserID, PrerequisiteContainerID).

% RULE 6: Content with High Average Rating (setof version)
% Helper predicate that might produce duplicates if is_subclass is non-deterministic
compute_content_high_average_rating_internal(ContentID, AverageRating) :-
    entity(ContentID, ContentType, _),
    is_subclass(ContentType, 'Container'), % This is where non-determinism caused duplicates
    findall(RatingVal,
            rel(rated, _, ContentID, [attr(ratingValue, RatingVal)]),
            Ratings),
    Ratings \= [],
    sum_list(Ratings, SumOfRatings),
    length(Ratings, NumberOfRatings),
    AverageRating is SumOfRatings / NumberOfRatings,
    HighRatingThreshold = 4.0,
    AverageRating >= HighRatingThreshold.

% Main predicate using setof to get unique results
content_high_average_rating(ContentID, AverageRating) :-
    setof(CID-AvgR, compute_content_high_average_rating_internal(CID, AvgR), UniquePairs),
    member(ContentID-AverageRating, UniquePairs).

% RULE 7: User Suitable for Advanced Content
user_suitable_for_advanced_content(UserID, AdvancedContainerID) :-
    entity(UserID, 'User', _),
    entity(AdvancedContainerID, AdvContType, Attributes),
    is_subclass(AdvContType, 'Container'),
    member(attr(difficulty, advanced), Attributes),

    forall( (rel(requires, AdvancedContainerID, PrereqID, _),
             entity(PrereqID, PrereqType, _), is_subclass(PrereqType, 'Container')),
            learner_mastered_container(UserID, PrereqID) ),

    content_covers_skill(AdvancedContainerID, SkillCoveredID),
    IntermediateSkillThreshold = 6,
    (   rel(hasSkill, UserID, SkillCoveredID, [attr(level, UserLevel)]),
        UserLevel >= IntermediateSkillThreshold
    ;
        \+ rel(hasSkill, UserID, SkillCoveredID, _)
    ).

% RULE 8: Stale Content
stale_content(ContentID) :-
    (   entity(ContentID, ContType, _), is_subclass(ContType, 'Container')
    ;   entity(ContentID, ToolType, _), is_subclass(ToolType, 'AssessmentTool')
    ),
    \+ rel(evolvesTo, ContentID, _, []),
    \+ rel(instanceOf, _, ContentID, []).

% RULE 9: Potential Collaborative Project Team
potential_project_team(ProjectID, User1, User2) :-
    entity(ProjectID, 'Project', [attr(requiredSkill, RequiredSkillID)]),
    rel(interestedInProject, User1, ProjectID, []),
    rel(interestedInProject, User2, ProjectID, []),
    User1 @< User2, % Using @< for canonical ordering
    (   (rel(hasSkill, User1, RequiredSkillID, [attr(level, Level1)]), Level1 > 5)
    ;   (rel(hasSkill, User2, RequiredSkillID, [attr(level, Level2)]), Level2 > 5)
    ;   (rel(hasSkill, User1, RequiredSkillID, _), rel(hasSkill, User2, RequiredSkillID, _))
    ).

% RULE 10: Most Active Contributor for a Skill
user_skill_contribution_count(UserID, SkillID, Count) :-
    entity(UserID, 'User', _),
    entity(SkillID, SkillCatType, _), is_subclass(SkillCatType, 'Category'),
    findall(teaching(EventID),
            (   rel(involvedInEvent, UserID, EventID, [attr(role, Role)]),
                member(Role, ['Teacher', 'Speaker']),
                rel(instanceOf, EventID, TaughtContainerID, _),
                content_covers_skill(TaughtContainerID, SkillID)
            ), TeachingActs),
    findall(creation(DevContainerID),
            (   rel(developed, UserID, DevContainerID, [attr(role, DevRole)]),
                member(DevRole, ['Creator', 'InstructionalDesigner']),
                (   entity(DevContainerID, DevContType, _), is_subclass(DevContType, 'Container')
                ;   entity(DevContainerID, DevToolType, _), is_subclass(DevToolType, 'AssessmentTool')
                ),
                content_covers_skill(DevContainerID, SkillID)
            ), CreationActs),
    length(TeachingActs, TeachCount),
    length(CreationActs, CreateCount),
    Count is TeachCount + CreateCount,
    Count > 0.

most_active_contributor_for_skill(SkillID, UserID, MaxCount) :-
    entity(SkillID, SkillCatType, _), is_subclass(SkillCatType, 'Category'),
    user_skill_contribution_count(UserID, SkillID, MaxCount),
    \+ ( user_skill_contribution_count(OtherUserID, SkillID, OtherCount),
         OtherUserID \= UserID,
         OtherCount > MaxCount ).

% RULE 11: Unpopular Content
unpopular_content(ContentID) :-
    entity(ContentID, ContType, _), is_subclass(ContType, 'Container'),
    (   (transitively_part_of(ContentID, CourseID), entity(CourseID, 'Course', _), rel(instanceOf, _, CourseID, _))
    ;   rel(instanceOf, _, ContentID, _)
    ),
    \+ content_high_average_rating(ContentID, _), % Calls the setof version, which is fine
    (   findall(RVal, rel(rated, _, ContentID, [attr(ratingValue, RVal)]), Ratings),
        length(Ratings, NumRatings),
        LowRatingCountThreshold = 2,
        NumRatings < LowRatingCountThreshold
    ;   \+ rel(rated, _, ContentID, _)
    ).

% RULE 12: Skill Gap in a Course
skill_gap_in_course(CourseID, SkillID, PercentageLackingSkill) :-
    entity(CourseID, 'Course', _),
    entity(SkillID, SkillCatType, _), is_subclass(SkillCatType, 'Category'),
    (   content_covers_skill(CourseID, SkillID)
    ;   (transitively_part_of(PartID, CourseID), content_covers_skill(PartID, SkillID))
    ),
    findall(LearnerID,
            (   rel(involvedInEvent, LearnerID, EventID, [attr(role, 'Learner')]),
                rel(instanceOf, EventID, EventContainerID, _),
                (   EventContainerID = CourseID
                ;   transitively_part_of(EventContainerID, CourseID)
                )
            ), EnrolledLearnersDup),
    sort(EnrolledLearnersDup, EnrolledLearners),
    EnrolledLearners \= [],
    findall(LearnerWithSkill,
            (   member(LearnerWithSkill, EnrolledLearners),
                rel(hasSkill, LearnerWithSkill, SkillID, [attr(level, Level)]),
                ProficiencyThreshold = 5,
                Level >= ProficiencyThreshold
            ), ProficientLearners),
    length(EnrolledLearners, TotalLearners),
    length(ProficientLearners, NumProficient),
    NumLacking is TotalLearners - NumProficient,
    (TotalLearners > 0 -> PercentageLackingSkill is round((NumLacking * 100) / TotalLearners) ; PercentageLackingSkill = 0),
    SignificantGapThreshold = 50,
    PercentageLackingSkill >= SignificantGapThreshold.

% RULE 13: Learning Path Suggestion
suggest_next_container(UserID, NextContainerID) :-
    entity(UserID, 'User', _),
    learner_mastered_container(UserID, MasteredContainerID),
    get_attribute_value(MasteredContainerID, difficulty, MasteredDifficulty),

    entity(NextContainerID, NextContType, NextAttributes), is_subclass(NextContType, 'Container'),
    NextContainerID \= MasteredContainerID,
    \+ learner_mastered_container(UserID, NextContainerID),

    (   (rel(partOf, MasteredContainerID, ParentID, _),
         rel(partOf, NextContainerID, ParentID, _),
         NextContainerID \= MasteredContainerID)
    ;   rel(requires, NextContainerID, MasteredContainerID, [])
    ;   (member(attr(difficulty, NextDifficulty), NextAttributes),
         ( (MasteredDifficulty = beginner, NextDifficulty = intermediate)
         ; (MasteredDifficulty = intermediate, NextDifficulty = advanced)
         ),
         (content_covers_skill(MasteredContainerID, SkillID), content_covers_skill(NextContainerID, SkillID)))
    ),
    forall( (rel(requires, NextContainerID, PrereqID, _), entity(PrereqID, PrereqContType, _), is_subclass(PrereqContType, 'Container')),
            learner_mastered_container(UserID, PrereqID) ).

% RULE 14: User with Broad Expertise
user_with_broad_expertise(UserID, NumberOfHighSkills) :-
    entity(UserID, 'User', _),
    HighSkillDegreeThreshold = 7,
    findall(SkillID,
            (   rel(hasSkill, UserID, SkillID, [attr(level, Level)]),
                Level >= HighSkillDegreeThreshold,
                entity(SkillID, SkillCatType, _), is_subclass(SkillCatType, 'Category')
            ), HighSkillIDs),
    sort(HighSkillIDs, UniqueHighSkillIDs),
    length(UniqueHighSkillIDs, NumberOfHighSkills),
    MinSkillsForBroadExpertise = 3,
    NumberOfHighSkills >= MinSkillsForBroadExpertise.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% QUERY EXAMPLES (Unchanged from previous, still valid)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
% RULE 1: Peer Support Opportunity
% ?- peer_support_opportunity(u3, u5, les_recursion, skill_prolog).

% RULE 2: Risk of Using Superseded Content
% ?- risk_of_superseded_content_use(qn2_old, qn2, Reason).

% RULE 3: Course of Study Completion Status
% ?- user_completed_course_of_study(u3, cos101).

% RULE 4: Overqualified Instructor for Introductory Content
% ?- overqualified_instructor_for_lesson(u4, les_recursion, skill_prolog).
% (or skill_recursion if data adjusted)

% RULE 5: Learner Lacks Prerequisite
% ?- learner_lacks_prerequisite(u5, workshop_advanced_prolog, mod_prolog).

% RULE 6: Content with High Average Rating
% ?- content_high_average_rating(ContentID, AvgRating).

% RULE 7: User Suitable for Advanced Content
% ?- user_suitable_for_advanced_content(u4, workshop_advanced_prolog).

% RULE 8: Stale Content
% ?- stale_content(mod_prolog_2005).

% RULE 9: Potential Collaborative Project Team
% ?- potential_project_team(p1, User1, User2).

% RULE 10: Most Active Contributor for a Skill
% FIX THIS RULE
% ?- most_active_contributor_for_skill(skill_prolog, User, Count).

% RULE 11: Unpopular Content
% ?- unpopular_content(mod_cpp).

% RULE 12: Skill Gap in a Course
% ?- skill_gap_in_course(cos101, skill_search, PercentageLacking).

% RULE 13: Learning Path Suggestion
% ?- suggest_next_container(u3, NextUp).

% RULE 14: User with Broad Expertise
% ?- user_with_broad_expertise(User, NumSkills).
*/