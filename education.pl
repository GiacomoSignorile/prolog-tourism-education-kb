:- use_module(library(lists)).
:- use_module(library(apply)). % For maplist

% --- Discontiguous Declarations ---
:- discontiguous inverse_of/2.
:- discontiguous subclass_of/2.
:- discontiguous entity/3.
:- discontiguous rel/4.

% --- DYNAMIC PREDICATE DECLARATION
:- dynamic strong_project_candidate/3. 

% --- Indexing for rel/4 and entity/3 ---
:- index(rel(1,1,1,0)).
:- index(entity(1,1,0)).

% Specific Info Predicates
:- discontiguous user_info/2.
:- discontiguous container_info/2. % For Course, Module, Lesson, Workshop, AssessmentContainer
:- discontiguous assessment_tool_info/2.
:- discontiguous category_info/2.    % For Skill, Topic
:- discontiguous event_info/2.
:- discontiguous accomplishment_info/2.
:- discontiguous project_info/2.

% Schema Predicates (Entity Attribute Schemas & Relationship Schemas)

:- discontiguous is_subclass_normal/2. 
:- discontiguous is_subclass_inverse/2.
:- table content_covers_skill/2. 

% --- SCHEMA DEFINITIONS ---

% -- Entities Hierarchy ('subclass_of/2') --
subclass_of('User', 'DomainEntity').
subclass_of('Container', 'DomainEntity').
subclass_of('AssessmentTool', 'DomainEntity').
subclass_of('Category', 'DomainEntity').
subclass_of('Event', 'DomainEntity').
subclass_of('Accomplishment', 'DomainEntity').
subclass_of('Project', 'DomainEntity').

% Subclasses of Container
subclass_of('Course', 'Container').
subclass_of('Module', 'Container').
subclass_of('Lesson', 'Container').
subclass_of('Workshop', 'Container').
subclass_of('AssessmentContainer', 'Container'). % A container that is also an assessment

% Subclasses of Category
subclass_of('Skill', 'Category').
subclass_of('Topic', 'Category').

% -- Entity Attribute Schemas ('EntityName'([attributeList])) --
% These define the *direct* attributes of each entity type.
'DomainEntity'([]). % Root entity, might have no direct attributes itself.
'User'([name]). % name is primary, other attributes could be role, status etc.
'Container'([name, language, difficulty]).
'Course'([]). % Inherits from Container, may have course-specific attrs like credits
'Module'([]). % Inherits from Container
'Lesson'([]). % Inherits from Container
'Workshop'([]). % Inherits from Container
'AssessmentContainer'([]). % Inherits from Container, is also an assessment
'AssessmentTool'([name, language, version]).
'Category'([name]). % For Skill, Topic
'Skill'([]). % Inherits from Category
'Topic'([]). % Inherits from Category
'Event'([name, startDate, endDate]). % Example attributes for an event
'Accomplishment'([name]).
'Project'([name, requiredSkill]). % requiredSkill is an attribute of the project entity

% -- Relationship Inverse Definitions ('inverse_of/2') --
inverse_of(partOf, hasPart).
inverse_of(instanceOf, hasInstance).
inverse_of(involvedInEvent, eventHasParticipant).
inverse_of(hasSkill, skillOfUser).
inverse_of(assessesSkill, skillAssessedBy).
inverse_of(evolvesTo, evolvesFrom).
inverse_of(requires, requiredBy).
inverse_of(ownsAccomplishment, accomplishmentOwnedBy).
inverse_of(awardedFor, awardsAccomplishment).
inverse_of(developed, developedBy).
inverse_of(interestedInProject, projectHasInterestedUser).
inverse_of(rated, ratingOf).

% -- Relationship Signature Schemas ('RelName'([Domain-RangePairs], [attributeList])) --
'partOf'(['Container'-'Container', 'AssessmentContainer'-'Container', 'Lesson'-'Module', 'Module'-'Course'], []).
'hasPart'(['Container'-'Container', 'Container'-'AssessmentContainer', 'Module'-'Lesson', 'Course'-'Module'], []).
'instanceOf'(['Event'-'Container'], []).
'hasInstance'(['Container'-'Event'], []).
'involvedInEvent'(['User'-'Event'], [role, startDate, endDate]).
'eventHasParticipant'(['Event'-'User'], [role, startDate, endDate]).
'hasSkill'(['User'-'Category'], [level]).
'skillOfUser'(['Category'-'User'], [level]).
'assessesSkill'(['Container'-'Category'], [scale]).
'skillAssessedBy'(['Category'-'Container'], [scale]).
'evolvesTo'(['AssessmentTool'-'AssessmentTool', 'Container'-'Container'], []).
'evolvesFrom'(['AssessmentTool'-'AssessmentTool', 'Container'-'Container'], []).
'requires'(['Container'-'Container', 'Container'-'AssessmentTool'], []).
'requiredBy'(['Container'-'Container', 'AssessmentTool'-'Container'], []).
'ownsAccomplishment'(['User'-'Accomplishment'], [score, issueDate, expiryDate, isValid]).
'accomplishmentOwnedBy'(['Accomplishment'-'User'], [score, issueDate, expiryDate, isValid]).
'awardedFor'(['Accomplishment'-'Container'], []).
'awardsAccomplishment'(['Container'-'Accomplishment'], []).
'developed'(['User'-'Container', 'User'-'AssessmentTool'], [role, date, version]).
'developedBy'(['Container'-'User', 'AssessmentTool'-'User'], [role, date, version]).
'interestedInProject'(['User'-'Project'], []).
'projectHasInterestedUser'(['Project'-'User'], []).
'rated'(['User'-'Container'], [ratingValue]).
'ratingOf'(['Container'-'User'], [ratingValue]).


% --- GENERIC SCHEMA UTILITY RULES  ---

% is_subclass(?Subclass, ?Superclass)
is_subclass(Class, Class) :- schema_entity_definition(Class, _), !. % Reflexivity: A class is a subclass of itself.
is_subclass(Subclass, Superclass) :-
  is_subclass_normal(Subclass, Superclass).
% is_subclass_inverse part from tourism example can be complex if not all relationships are classes.
% For now, keeping the simpler is_subclass structure that worked. If needed, can add is_subclass_inverse.

is_subclass_normal(Class, Class) :- schema_entity_definition(Class, _), !. 
is_subclass_normal(Subclass, Superclass) :-
  subclass_of(Subclass, Superclass).
is_subclass_normal(Subclass, Superclass) :-
  subclass_of(Subclass, Middleclass),
  is_subclass_normal(Middleclass, Superclass).

% schema_entity_definition(ClassName, AttributeList)
schema_entity_definition(ClassName, AttributeList) :-
    atom(ClassName), % Good to keep this check
    defined_entity_schema(ClassName, AttributeList).

% The dispatcher predicate
defined_entity_schema('User', Attributes) :- 'User'(Attributes).
defined_entity_schema('Container', Attributes) :- 'Container'(Attributes).
defined_entity_schema('Course', Attributes) :- 'Course'(Attributes).
defined_entity_schema('Module', Attributes) :- 'Module'(Attributes).
defined_entity_schema('Lesson', Attributes) :- 'Lesson'(Attributes).
defined_entity_schema('Workshop', Attributes) :- 'Workshop'(Attributes).
defined_entity_schema('AssessmentContainer', Attributes) :- 'AssessmentContainer'(Attributes).
defined_entity_schema('AssessmentTool', Attributes) :- 'AssessmentTool'(Attributes).
defined_entity_schema('Category', Attributes) :- 'Category'(Attributes).
defined_entity_schema('Skill', Attributes) :- 'Skill'(Attributes).
defined_entity_schema('Topic', Attributes) :- 'Topic'(Attributes).
defined_entity_schema('Event', Attributes) :- 'Event'(Attributes).
defined_entity_schema('Accomplishment', Attributes) :- 'Accomplishment'(Attributes).
defined_entity_schema('Project', Attributes) :- 'Project'(Attributes).
defined_entity_schema('DomainEntity', Attributes) :- 'DomainEntity'(Attributes). 

% schema_relationship_definition(RelName, SubjObjList, AttrList)
schema_relationship_definition(RelName, SubjObjList, AttrList) :-
    atom(RelName),
    defined_relationship_schema(RelName, SubjObjList, AttrList).

% The dispatcher predicate for relationship schemas
defined_relationship_schema('partOf', DomainRangePairs, Attributes) :-
    'partOf'(DomainRangePairs, Attributes).
defined_relationship_schema('hasPart', DomainRangePairs, Attributes) :-
    'hasPart'(DomainRangePairs, Attributes).
defined_relationship_schema('instanceOf', DomainRangePairs, Attributes) :-
    'instanceOf'(DomainRangePairs, Attributes).
defined_relationship_schema('hasInstance', DomainRangePairs, Attributes) :-
    'hasInstance'(DomainRangePairs, Attributes).
defined_relationship_schema('involvedInEvent', DomainRangePairs, Attributes) :-
    'involvedInEvent'(DomainRangePairs, Attributes).
defined_relationship_schema('eventHasParticipant', DomainRangePairs, Attributes) :-
    'eventHasParticipant'(DomainRangePairs, Attributes).
defined_relationship_schema('hasSkill', DomainRangePairs, Attributes) :-
    'hasSkill'(DomainRangePairs, Attributes).
defined_relationship_schema('skillOfUser', DomainRangePairs, Attributes) :-
    'skillOfUser'(DomainRangePairs, Attributes).
defined_relationship_schema('assessesSkill', DomainRangePairs, Attributes) :-
    'assessesSkill'(DomainRangePairs, Attributes).
defined_relationship_schema('skillAssessedBy', DomainRangePairs, Attributes) :-
    'skillAssessedBy'(DomainRangePairs, Attributes).
defined_relationship_schema('evolvesTo', DomainRangePairs, Attributes) :-
    'evolvesTo'(DomainRangePairs, Attributes).
defined_relationship_schema('evolvesFrom', DomainRangePairs, Attributes) :-
    'evolvesFrom'(DomainRangePairs, Attributes).
defined_relationship_schema('requires', DomainRangePairs, Attributes) :-
    'requires'(DomainRangePairs, Attributes).
defined_relationship_schema('requiredBy', DomainRangePairs, Attributes) :-
    'requiredBy'(DomainRangePairs, Attributes).
defined_relationship_schema('ownsAccomplishment', DomainRangePairs, Attributes) :-
    'ownsAccomplishment'(DomainRangePairs, Attributes).
defined_relationship_schema('accomplishmentOwnedBy', DomainRangePairs, Attributes) :-
    'accomplishmentOwnedBy'(DomainRangePairs, Attributes).
defined_relationship_schema('awardedFor', DomainRangePairs, Attributes) :- 
    'awardedFor'(DomainRangePairs, Attributes).
defined_relationship_schema('awardsAccomplishment', DomainRangePairs, Attributes) :-
    'awardsAccomplishment'(DomainRangePairs, Attributes).
defined_relationship_schema('developed', DomainRangePairs, Attributes) :-
    'developed'(DomainRangePairs, Attributes).
defined_relationship_schema('developedBy', DomainRangePairs, Attributes) :-
    'developedBy'(DomainRangePairs, Attributes).
defined_relationship_schema('interestedInProject', DomainRangePairs, Attributes) :-
    'interestedInProject'(DomainRangePairs, Attributes).
defined_relationship_schema('projectHasInterestedUser', DomainRangePairs, Attributes) :-
    'projectHasInterestedUser'(DomainRangePairs, Attributes).
defined_relationship_schema('rated', DomainRangePairs, Attributes) :-
    'rated'(DomainRangePairs, Attributes).
defined_relationship_schema('ratingOf', DomainRangePairs, Attributes) :-
    'ratingOf'(DomainRangePairs, Attributes).

% invert_relationship(+RelationshipName, -InvertedRelationshipClause)
% Returns the schema clause (Name, Refs, Attrs) of the inverse.
invert_relationship(RelationshipName, InvertedRelationshipClause) :-
  inverse_of(InvertedRelationshipName, RelationshipName),
  schema_relationship_definition(RelationshipName, SubjObjList, AttributeList),
  !,
  invert_subj_obj(SubjObjList, InvertedSubjObjList),
  InvertedRelationshipClause =.. [InvertedRelationshipName, InvertedSubjObjList, AttributeList].
invert_relationship(InvertedRelationshipName, RelationshipClause) :-
  inverse_of(InvertedRelationshipName, RelationshipName),
  schema_relationship_definition(RelationshipName, SubjObjList, AttributeList),
  !,
  RelationshipClause =.. [RelationshipName, SubjObjList, AttributeList].
invert_relationship(RelationshipName, RelationshipClause) :- % No inverse defined
  \+ inverse_of(_, RelationshipName),
  \+ inverse_of(RelationshipName, _),
  schema_relationship_definition(RelationshipName, SubjObjList, AttributeList),
  RelationshipClause =.. [RelationshipName, SubjObjList, AttributeList].

invert_subj_obj([], []).
invert_subj_obj([Subject-Object|T1], [Object-Subject|T2]) :-
  invert_subj_obj(T1, T2).

% --- ATTRIBUTE INHERITANCE ---
% Ensures attributes are inherited from all superclasses, not just direct ones, and avoids duplicates
gather_attributes(ClassName, AllAttributes) :-
    atom(ClassName),
    findall(Super, is_subclass(ClassName, Super), SupersWithSelf),
    maplist(schema_entity_definition_or_empty, SupersWithSelf, AttrLists),
    flatten(AttrLists, FlatAttrs),
    list_to_set(FlatAttrs, AllAttributes).

schema_entity_definition_or_empty(Class, Attrs) :-
    schema_entity_definition(Class, Attrs), !.
schema_entity_definition_or_empty(_, []).

% --- VALIDATION RULES ---
% Ensures that all rel(partOf, X, Y, _) facts have both X and Y defined as entities.
% Can be run to check for missing entities in partOf relationships
validate_partof_entities :-
    forall(
        rel(partOf, X, Y, _),
        ( (entity(X, _, _) -> true ; format('WARNING: partOf relationship source ~w is not a defined entity!~n', [X])),
          (entity(Y, _, _) -> true ; format('WARNING: partOf relationship target ~w is not a defined entity!~n', [Y]))
        )
    ).

% gather_references and its helpers would be similar if needed for relationships.

% --- Generic Helper Rules ---
% get_attribute_value/3 
get_attribute_value(EntityID, AttrName, Value) :-
    entity(EntityID, _, Attributes),
    member(attr(AttrName, Value), Attributes).
get_attribute_value(EntityID, AttrName, default) :-
    entity(EntityID, _, _),
    \+ (entity(EntityID, _, Attributes), member(attr(AttrName, _), Attributes)).

transitively_part_of(Component, Container) :- rel(partOf, Component, Container, _).
transitively_part_of(Component, Container) :-
    rel(partOf, Component, Intermediate, _),
    transitively_part_of(Intermediate, Container).

% content_covers_skill and ccs_check 
content_covers_skill(ContainerID, SkillID) :- % This rule checks if a Container covers a Skill.
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
    \+ (SubPartID == ContainerID),
    content_covers_skill(SubPartID, SkillID).

learner_mastered_container(UserID, ContainerID) :- % This rule checks if a user has mastered a container.
    entity(UserID, 'User', _),
    entity(ContainerID, ContainerType, _), is_subclass(ContainerType, 'Container'),
    rel(ownsAccomplishment, UserID, AccID, Attributes),
    member(attr(isValid, true), Attributes),
    entity(AccID, 'Accomplishment', _),
    rel(awardedFor, AccID, ContainerID, _).

% --- INSTANCE FACTS ---

% -- Specific Info Predicates --
user_info(u1, 'Alice'). % ... all user_info facts ...
user_info(u2, 'Bob_Teacher').
user_info(u3, 'Charlie_Learner').
user_info(u4, 'David_Mentor').
user_info(u5, 'Eve_Student').
user_info(u6, 'Fiona_Expert').
user_info(u7, 'Charles_Teacher').

container_info(cos101, 'AI Course'). % ... all container_info facts ...
container_info(mod_cpp, 'C++ Module').
container_info(mod_prolog, 'Prolog Module').
container_info(mod_search, 'Search Algos Module').
container_info(les_recursion, 'Recursion Lesson').
container_info(asm_prolog_quiz, 'Prolog Quiz').
container_info(workshop_advanced_prolog, 'Advanced Prolog Workshop').
container_info(mod_prolog_2005, 'Prolog Lesson 2005/06').

assessment_tool_info(qn1, 'Prolog Concepts QNR'). % ... all assessment_tool_info facts ...
assessment_tool_info(qn2, 'Search Algo QNR').
assessment_tool_info(qn2_old, 'Search Algo QNR Old').

category_info(skill_prolog, 'Prolog Programming'). % ... all category_info facts ...
category_info(skill_search, 'Search Algorithms').
category_info(skill_recursion, 'Recursion').
category_info(skill_expert_prolog, 'Expert Prolog Techniques').
category_info(topic_ai_ethics, 'AI Ethics').

event_info(evt_prolog_les, 'Prolog Lesson Event'). % ... all event_info facts ...
event_info(evt_search_mod, 'Search Module Event').
event_info(evt_ai_course_active, 'AI Course Active Instance').
event_info(evt_adv_prolog_ws, 'Advanced Prolog Workshop Event').

accomplishment_info(ac_prolog_module_cert, 'Prolog Module Certificate'). % ... all accomplishment_info facts ...
accomplishment_info(ac_search_module_pass, 'Search Module Pass').
accomplishment_info(ac_expert_prolog_badge, 'Expert Prolog Badge').
accomplishment_info(ac_adv_prolog_ws_cert, 'Advanced Prolog Workshop Certificate').
accomplishment_info(ac_basic_mod_prolog, 'Basic Prolog Module Pass').

project_info(p1, 'AI Chatbot'). % ... all project_info facts ...
project_info(p2, 'Game Pathfinding').

% -- Generic Entity Facts ('entity/3') --
entity(u1, 'User', [attr(name, 'Alice')]). % ... all entity facts ...
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
entity(ac_adv_prolog_ws_cert, 'Accomplishment', [attr(name, 'Advanced Prolog Workshop Certificate')]).
entity(ac_basic_mod_prolog, 'Accomplishment', [attr(name, 'Basic Prolog Module Pass')]).

entity(p1, 'Project', [attr(name, 'AI Chatbot'), attr(requiredSkill, skill_prolog)]).
entity(p2, 'Project', [attr(name, 'Game Pathfinding'), attr(requiredSkill, skill_search)]).

% -- Generic Relationship Facts ('rel/4') --
rel(partOf, mod_prolog, cos101, []). % ... all rel facts ...
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
rel(assessesSkill, workshop_advanced_prolog, skill_expert_prolog, []).
rel(assessesSkill, mod_search, skill_search, [attr(scale, 'pass_fail_scale')]).

rel(evolvesTo, qn2_old, qn2, []).
rel(requires, mod_search, qn2_old, []).
rel(requires, cos101, mod_prolog, []).
rel(requires, workshop_advanced_prolog, mod_prolog, []).

rel(ownsAccomplishment, u3, ac_prolog_module_cert, [attr(score, 85), attr(issueDate, '2023-01-01'), attr(expiryDate, '2023-02-28'), attr(isValid, true)]).
rel(ownsAccomplishment, u4, ac_prolog_module_cert, [attr(score, 95), attr(issueDate, '2022-01-01'), attr(expiryDate, '2022-02-28'), attr(isValid, true)]).
rel(ownsAccomplishment, u6, ac_expert_prolog_badge, [attr(issueDate, '2023-05-01'), attr(isValid, true)]).
rel(ownsAccomplishment, u7, ac_search_module_pass, [attr(issueDate, '2024-05-10'), attr(isValid, true)]).
rel(ownsAccomplishment, u3, ac_adv_prolog_ws_cert, [attr(score, 95), attr(issueDate, '2023-06-01'), attr(isValid, true)]).
rel(ownsAccomplishment, u5, ac_basic_mod_prolog, [attr(score, 80), attr(issueDate, '2023-01-10'), attr(isValid, true)]).

rel(awardedFor, ac_prolog_module_cert, mod_prolog, []).
rel(awardedFor, ac_search_module_pass, mod_search, []).
rel(awardedFor, ac_expert_prolog_badge, workshop_advanced_prolog, []).
rel(awardedFor, ac_adv_prolog_ws_cert, workshop_advanced_prolog, []).
rel(awardedFor, ac_basic_mod_prolog, mod_prolog, []).

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


% Rule 1: Identify if a user is a learner in any event.
% A user is considered a learner if they have the 'Learner' role in any event they are involved in.
is_learner(User) :-
    entity(User, 'User', _), % Ensure User is a User entity
    rel(involvedInEvent, User, _Event, Attributes),
    member(attr(role, 'Learner'), Attributes).

% Rule 2: Identify if a user is a teacher or speaker in any event.
% A user is considered a teacher if they have the 'Teacher' or 'Speaker' role in any event.
is_teacher(User) :-
    entity(User, 'User', _),
    rel(involvedInEvent, User, _Event, Attributes),
    ( member(attr(role, 'Teacher'), Attributes) ; member(attr(role, 'Speaker'), Attributes) ).

% Rule 3: Identify if a user is a content developer.
% A user is a content developer if they have developed any Container or AssessmentTool.
is_content_developer(User) :-
    entity(User, 'User', _),
    rel(developed, User, DevelopedThing, _),
    ( entity(DevelopedThing, Type, _), is_subclass(Type, 'Container') ;
      entity(DevelopedThing, Type, _), is_subclass(Type, 'AssessmentTool') ).

% Rule 4: Get the skill level of a user for a specific skill.
% Retrieves the level attribute from the hasSkill relationship.
user_has_skill_level(User, Skill, Level) :-
    entity(User, 'User', _),
    entity(Skill, SkillType, _), is_subclass(SkillType, 'Skill'), % Or 'Category' if topics can have levels
    rel(hasSkill, User, Skill, Attributes),
    member(attr(level, Level), Attributes).

% Rule 5: Check if a user's skill level is below a required threshold.
% Useful for identifying skill gaps.
user_lacks_skill_for_level(User, Skill, RequiredLevel) :-
    entity(User, 'User', _),
    entity(Skill, SkillType, _), is_subclass(SkillType, 'Skill'),
    (   user_has_skill_level(User, Skill, CurrentLevel) ->
        CurrentLevel < RequiredLevel
    ;   % User does not have the skill listed at all, so they lack it for any positive RequiredLevel
        RequiredLevel > 0,
        \+ rel(hasSkill, User, Skill, _)
    ).

% Rule 6: Identify the skill required by a project.
% Retrieves the requiredSkill attribute from the Project entity.
project_requires_skill(ProjectID, SkillID) :-
    entity(ProjectID, 'Project', Attributes),
    member(attr(requiredSkill, SkillID), Attributes),
    entity(SkillID, SkillType, _), is_subclass(SkillType, 'Skill'). % Ensure it's a skill

% Rule 7: Check if a user meets the skill requirement for a project (at a minimum level).
% Assumes a minimum proficiency level is needed.
user_meets_project_skill_requirement(User, Project, MinLevel) :-
    project_requires_skill(Project, SkillID),
    user_has_skill_level(User, SkillID, UserLevel),
    UserLevel >= MinLevel.

% Rule 8: Predict project interest based on mastered skills (ILP-like hypothesis).
% This rule implements a hypothesis an ILP system might learn:
% If a user has mastered a container, and that container covers a skill,
% and a project requires that skill, then the user might be interested in the project.
predict_project_interest_from_skills(User, Project) :-
    entity(User, 'User', _),
    entity(Project, 'Project', _),
    learner_mastered_container(User, Container),       % User has mastered a container
    content_covers_skill(Container, Skill),           % That container covers a certain skill
    project_requires_skill(Project, Skill),           % A project requires this skill
    \+ rel(interestedInProject, User, Project, _).    % And user is not already listed as interested

% Rule 9: Suggest learning material for a user to improve a specific skill.
% Suggests a container if the user has a low level or lacks the skill,
% the container covers the skill, and the user hasn't already mastered it.
suggest_learning_material_for_skill_gap(User, Container, SkillToImprove) :-
    entity(User, 'User', _),
    entity(SkillToImprove, SkillType, _), is_subclass(SkillType, 'Skill'),
    user_lacks_skill_for_level(User, SkillToImprove, 5), % Example: target level 5
    content_covers_skill(Container, SkillToImprove),
    entity(Container, CType, _), is_subclass(CType, 'Container'),
    \+ learner_mastered_container(User, Container).

% Rule 10: Find potential collaborators for a project.
% Lists all users who have expressed interest in a given project.
find_project_team_candidates(Project, UserList) :-
    entity(Project, 'Project', _),
    findall(User, rel(interestedInProject, User, Project, _), UserList).

% Rule 11: Check if a user attended an event instance of a container.
% This is about participation, not necessarily mastery.
user_attended_container_event(User, Container) :-
    entity(User, 'User', _),
    entity(Container, CType, _), is_subclass(CType, 'Container'),
    rel(instanceOf, Event, Container, _),          % Find an event that is an instance of the Container
    rel(involvedInEvent, User, Event, _).          % Check if the User was involved in that Event

% Rule 12: Identify an instructor for a specific container.
% An instructor is someone who taught an event for the container or developed it.
is_instructor_for_container(Instructor, Container) :-
    entity(Instructor, 'User', _),
    entity(Container, CType, _), is_subclass(CType, 'Container'),
    (   rel(instanceOf, Event, Container, _),
        rel(involvedInEvent, Instructor, Event, Attributes),
        member(attr(role, 'Teacher'), Attributes)
    ;   rel(developed, Instructor, Container, _)
    ).

% Rule 13: Get all recursive parts of a container (e.g., modules in a course, lessons in a module).
% This uses the `transitively_part_of` but collects all such parts for a given top container.
% Note: `transitively_part_of(Component, Container)` means Component is part of Container.
% So we need to find X such that transitively_part_of(X, TopContainer).
get_recursive_container_parts(TopContainer, PartList) :-
    entity(TopContainer, TopCType, _), is_subclass(TopCType, 'Container'),
    findall(Part, (transitively_part_of(Part, TopContainer), Part \== TopContainer), DupPartList),
    list_to_set(DupPartList, PartList). % Remove duplicates

% Rule 14: Check if a user's specific accomplishment is currently valid.
% Uses the `isValid` attribute from the `ownsAccomplishment` relationship.
is_user_accomplishment_active(User, AccomplishmentID) :-
    entity(User, 'User', _),
    entity(AccomplishmentID, 'Accomplishment', _),
    rel(ownsAccomplishment, User, AccomplishmentID, Attributes),
    member(attr(isValid, true), Attributes).

% Rule 15: Identify if a user is an expert in a specific skill.
% Expertise is defined by having a skill level above a certain threshold.
is_skill_expert(User, Skill, ExpertThreshold) :-
    user_has_skill_level(User, Skill, UserLevel),
    UserLevel >= ExpertThreshold.

% Rule 16: Check if an assessment tool is the latest version.
% An assessment tool is current if it does not evolve to another tool.
is_tool_current_version(ToolID) :-
    entity(ToolID, 'AssessmentTool', _),
    \+ rel(evolvesTo, ToolID, _AnotherTool, _).

% Rule 17: Find containers by a specific difficulty rating.
% Retrieves containers that have a matching 'difficulty' attribute.
find_content_by_difficulty_rating(DifficultyAtom, ContainerList) :-
    atom(DifficultyAtom), % e.g., beginner, intermediate, advanced
    findall(ContainerID,
            (   entity(ContainerID, CType, Attributes),
                is_subclass(CType, 'Container'),
                member(attr(difficulty, DifficultyAtom), Attributes)
            ),
            ContainerList).

% Rule 18: Get the list of immediate prerequisites for a container.
% Finds all containers C_prereq such that 'Container' requires 'C_prereq'.
get_immediate_prerequisites_for_container(Container, PrerequisiteContainers) :-
    entity(Container, CType, _), is_subclass(CType, 'Container'),
    findall(Prereq, rel(requires, Container, Prereq, _), PrerequisiteContainers).

% Rule 19: Get all transitive prerequisites for a container (Search Technique).
% This rule recursively finds all containers that are direct or indirect prerequisites.
% It uses a depth-first search approach to find all prerequisites.
get_all_prerequisites_for_container(Container, AllPrerequisiteContainers) :-
    entity(Container, CType, _), is_subclass(CType, 'Container'),
    findall(P, collect_all_prereqs_recursive(Container, P, [Container]), PrereqsListDups),
    list_to_set(PrereqsListDups, AllPrerequisiteContainers).

collect_all_prereqs_recursive(CurrentContainer, Prereq, _Visited) :-
    rel(requires, CurrentContainer, Prereq, _), % Direct prerequisite
    \+ entity(Prereq, 'AssessmentTool', _). % Assuming prerequisites are other containers
collect_all_prereqs_recursive(CurrentContainer, Prereq, Visited) :-
    rel(requires, CurrentContainer, IntermediatePrereq, _),
    \+ entity(IntermediatePrereq, 'AssessmentTool', _),
    \+ member(IntermediatePrereq, Visited), % Avoid cycles and redundant exploration
    collect_all_prereqs_recursive(IntermediatePrereq, Prereq, [IntermediatePrereq | Visited]).

% Rule 20: Find an expert user who has also created content relevant to their expertise skill (Complex Query/Search).
% This rule searches for users who are experts in a skill AND have developed content that covers that same skill.
find_expert_content_creator_for_skill(User, Skill, CreatedContent) :-
    is_skill_expert(User, Skill, 8), % Define expert threshold, e.g., level 8+
    rel(developed, User, CreatedContent, _),
    ( entity(CreatedContent, CCType, _), is_subclass(CCType, 'Container') ;
      entity(CreatedContent, CCType, _), is_subclass(CCType, 'AssessmentTool') ),
    content_covers_skill(CreatedContent, Skill).

% Rule 21 : Identify and Record Strong Project Candidates
% A user is a strong candidate for a project if:
% 1. They are interested in the project.
% 2. The project requires a specific skill.
% 3. The user possesses that skill.
% If these conditions are met and the user isn't already marked, assert them as a strong candidate.

identify_and_record_strong_candidates :-
    % Iterate through all users interested in projects
    rel(interestedInProject, User, Project, _),          % User is interested in Project
    project_requires_skill(Project, RequiredSkill),      % Project requires Skill
    rel(hasSkill, User, RequiredSkill, _),               % User possesses that RequiredSkill
    
    % Check if already recorded to avoid duplicate assertions and infinite loops if called repeatedly
    (   \+ strong_project_candidate(User, Project, RequiredSkill) ->
        assertz(strong_project_candidate(User, Project, RequiredSkill)),
        user_info(User, UserName),
        project_info(Project, ProjectName),
        category_info(RequiredSkill, SkillName),
        format('INFO: ~w (~w) is now a strong candidate for project ~w (~w) requiring skill ~w (~w).~n',
               [UserName, User, ProjectName, Project, SkillName, RequiredSkill])
    ;   true % Already recorded, do nothing
    ).

% Rule 22: Identify a Learner Potentially Stuck Due to Missing Prerequisites
% A learner is potentially stuck if they are actively involved in a container
% (e.g., an event instance) but have not mastered one of its immediate prerequisite containers.
is_learner_potentially_stuck(Learner, InContainer, missing_prerequisite(PrereqContainer)) :-
    entity(Learner, 'User', _),
    entity(InContainer, ICType, _), is_subclass(ICType, 'Container'),
    % Learner is actively involved in an event instance of InContainer
    rel(instanceOf, EventID, InContainer, _),
    rel(involvedInEvent, Learner, EventID, EventAttrs),
    member(attr(role, 'Learner'), EventAttrs),
    % InContainer requires a PrereqContainer
    rel(requires, InContainer, PrereqContainer, _),
    entity(PrereqContainer, PCType, _), is_subclass(PCType, 'Container'), % Ensure Prereq is a container
    % And the learner has NOT mastered this prerequisite
    \+ learner_mastered_container(Learner, PrereqContainer).

% Rule 23: Find a Peer for a Container Activity
% Finds another learner (Peer) who is involved in the same event instance of a specific container
% as the given User. This suggests a direct peer for collaborative activities.
find_peer_for_container_activity(User, Container, Peer) :-
    entity(User, 'User', _),
    entity(Container, CType, _), is_subclass(CType, 'Container'),
    entity(Peer, 'User', _),
    User \== Peer, % Ensure User and Peer are different
    % Both User and Peer are learners in the same event instance of the Container
    rel(instanceOf, EventID, Container, _),
    rel(involvedInEvent, User, EventID, UserEventAttrs),
    member(attr(role, 'Learner'), UserEventAttrs),
    rel(involvedInEvent, Peer, EventID, PeerEventAttrs),
    member(attr(role, 'Learner'), PeerEventAttrs).

% Rule 24: Calculate Average Rating for a Container
% Calculates the average rating and the number of ratings for a given container
% using the `aggregate_all/3` predicate for robust aggregation.
container_average_rating(ContainerID, AverageRating, NumRatings) :-
    entity(ContainerID, CType, _), is_subclass(CType, 'Container'),
    aggregate_all(
        count + sum(RatingValue), % Operations: count ratings, sum their values
        (   rel(rated, _User, ContainerID, RateAttrs), % For each rating relationship for this container
            member(attr(ratingValue, RatingValue), RateAttrs)
        ),
        NumRatings + SumOfRatings
    ),
    (NumRatings > 0 -> AverageRating is SumOfRatings / NumRatings ; AverageRating = 'not_rated', NumRatings = 0).

% Rule 25: Suggest Direct Next Step for Skill Development (Search-like)
% Suggests a container for a user to take next to develop a target skill.
% The suggested container must cover the skill, the user must not have mastered it yet,
% AND the user must have mastered all of its immediate container prerequisites.
suggest_direct_next_step_for_skill(User, TargetSkill, SuggestedContainer) :-
    entity(User, 'User', _),
    entity(TargetSkill, SKType, _), is_subclass(SKType, 'Skill'),
    entity(SuggestedContainer, SCType, _), is_subclass(SCType, 'Container'),
    
    content_covers_skill(SuggestedContainer, TargetSkill), % The container covers the target skill
    \+ learner_mastered_container(User, SuggestedContainer), % User hasn't mastered this container yet
    
    % Check if all immediate *container* prerequisites for SuggestedContainer are mastered by User
    % This uses forall/2: For all P that satisfy Goal1, P must also satisfy Goal2.
    forall(
        (   rel(requires, SuggestedContainer, Prereq, _), % Prereq is required by SuggestedContainer
            entity(Prereq, PrereqEntityClass, _),
            is_subclass(PrereqEntityClass, 'Container')   % And Prereq is itself a Container
        ),
        learner_mastered_container(User, Prereq)          % Then User must have mastered Prereq
    ),

    true. % Placeholder if no further conditions.


% ====================================================================================
% QUERIES 
% Query 1
% Who are all the learners?
% ?- setof(User-Name, (is_learner(User), user_info(User, Name)), Learners).

% Query 2
% Who are the teachers or speakers?
% ?- is_teacher(User), user_info(User, Name)

% Query 3
% Which users have 'Prolog Programming' skill and what is their level?
%?- user_has_skill_level(User, skill_prolog, Level), user_info(User, Name).

% Query 4
% What skill is required for the 'AI Chatbot' project (p1)?
% ?- project_requires_skill(p1, SkillID), category_info(SkillID, SkillName).

% Query 5
% Based on mastered skills, who might be interested in project 'p1' (AI Chatbot)? (ILP-like rule)
% ?- predict_project_interest_from_skills(User, p1), user_info(User, Name).

% query 6
% Who are the current candidates (already expressed interest) for the 'AI Chatbot' project (p1)?
% ?- find_project_team_candidates(p1, UserList).

% Query 7
% What containers are rated 'beginner' difficulty?
% ?- find_content_by_difficulty_rating(beginner, ContainerList).

% Query 8
% What are all the recursive parts (lessons, sub-modules) of the 'AI Course' (cos101)?
% ?- get_recursive_container_parts(cos101, PartList).

% Query 9
% Did 'Charlie_Learner' (u3) attend an event for the 'Recursion Lesson' (les_recursion)?
% ?- user_attended_container_event(u3, les_recursion).

% Query 10
% Who is an instructor for the 'Prolog Module' (mod_prolog)?
% ?- is_instructor_for_container(Instructor, mod_prolog), user_info(Instructor, Name).

% Query 11
% Is 'Charlie_Learner' (u3)'s 'Prolog Module Certificate' (ac_prolog_module_cert) currently valid?
% ?- is_user_accomplishment_active(u3, ac_prolog_module_cert).


% Query 12
% Is the assessment tool 'qn2' the current version?
% ?- is_tool_current_version(qn2).

% Query 13
% What are ALL prerequisites (direct and indirect) for the 'Advanced Prolog Workshop' (workshop_advanced_prolog)?
% ?- get_all_prerequisites_for_container(workshop_advanced_prolog, AllPrereqs).


% Query 14
% Find users who are expert (level >= 8) in 'Prolog Programming' (skill_prolog) AND have developed content that covers 'Prolog Programming'.
% ?- find_expert_content_creator_for_skill(User, skill_prolog, CreatedContent), user_info(User, UserName), container_info(CreatedContent, ContentName).

% Query 15
% You would typically call this rule once to process existing data:
% ?- identify_and_record_strong_candidates.
% Or, more likely, to find all and collect them (though the rule asserts, so findall isn't strictly for collection here):
% ?- findall(_, identify_and_record_strong_candidates, _).
% This ensures it attempts to process for all possible combinations from the KBs current state.

% Query 16
% Find learners potentially stuck in the 'Advanced Prolog Workshop' (workshop_advanced_prolog):
% ?- is_learner_potentially_stuck(Learner, workshop_advanced_prolog, Reason), user_info(Learner, Name).

% Query 17
% Find peers for 'Charlie_Learner' (u3) in the 'Advanced Prolog Workshop' (workshop_advanced_prolog):
% ?- find_peer_for_container_activity(u3, workshop_advanced_prolog, Peer), user_info(Peer, PeerName).

% Query 18
% Get the average rating for the 'Prolog Module' (mod_prolog):
% ?- container_average_rating(mod_prolog, AvgRating, NumRatings).

% Query 19
% Suggest the next container for 'Eve_Student' (u5) to learn 'Expert Prolog Techniques' (skill_expert_prolog):
% ? - suggest_direct_next_step_for_skill(u5, skill_expert_prolog, Suggested), container_info(Suggested, Name).
% Or if we want a list of suggestions:
% setof(Name-Suggested,( suggest_direct_next_step_for_skill(u5, skill_expert_prolog, Suggested), container_info(Suggested, Name)),UniqueSuggestions).


