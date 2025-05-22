:- use_module(library(lists)).
:- discontiguous inverse_of/2.
:- discontiguous subclass_of/2.
:- unknown(_, fail).


% ENTITIES

% Entities hierarchy

Organization
Club
Company
EducationalInstitution
Government
NonGovernmentalOrganization
ProfessionalAssociationUnion
Institution
PublicInstitution
SubGroup
Person
User
PersonUser
OrganizationUser
GroupUser

Artwork
Handicraft
IndustrialWork
Component
Device

Family
Group
Series
Vocabulary
Corpus

Category
Concept
Genre
Occupation
Period
Stuff
Subject
Trend
Language
Text
Sentence
Word

Audio
Music
Noise
Song
Speech
Printable
Book
Booklet
Card
Cartography
DesignDiagram
Leaflet
Letter
Magazine
Manual
MusicalScore
Picture
Postcard
Poster
TextDocument
HandPrintedBook
Video
Documentary
Movie
Article
Visual
Microform
Projection

Concert
Conference
Exhibition
Fair
HistoricalEvent
Lecture
Show

Algorithm
Approach
Discipline
Invention
ProgrammingLanguage
Standard
Regulation
Taxonomy
Technology
Theorem
TheoreticalModel
WorkOfArt
CartographicWork
Expression
MusicalWork

Domain
EMailAddress
Protocol
URI
URL

Identifier

Administrative
Continent
Country
Region/State
Province/County
Town
District
Road/Street/Square
Address
Building
Apartment
Castle
Cottage
House
Mansion
Skyscraper
Geographic
Area
Hill
Island
Lake
Land
Mountain
Peninsula
Planet
River
Sea
Valley
Beach
Basin
Butte
Canyon
Cave
Plateau

Activity
Case
Process
Task

Duration
TimePeriod
Time
TimeInterval
Timeline
TimePoint
Year
Month
Day
DateTime

PlaceToVisit
Archive
Architecture
Church
Library
Monument
Museum
Palace
PanoramicView
Park
Ruin
Theater
AmusementPark
ArchaeologicalSite
Cathedral
Citadel
Fort
Fortress
Fountain
Landmark
Monastery
Mosque
Pagoda
Plaza
Shrine
Stadium
Temple
Tower
Wat
Service
Bank
EatingDrinking
Bar
Pub
Bistro
Cafe
Brasserie
Canteen
SnackBar
Grillroom
Stakehouse
Pizzeria
Tavern
TeaHouse
Market
Restaurant
GasStation
Hotel
HotelService
BikeTour
CookingClass
CulturalTour
DiscoParty
FacialTreatments
HappyHour
Yoga
Pilates
WalkingTour
Aerobics
AquaticExercise
BeautyService
BarberService
Bath
Jacuzzi
Tub
Massage
Eastern
Thai
Turkish
Shiatsu
Western
Swedish
Detox
Collagen
Firming
Pedicure
Sunbath
Shower
Swiss
Vichy
Steam
Roman
Air
Spa
Medical
Resort
Therapy
Aroma
HealthService
Hospital
Pharmacy
PostOffice
PublicService
AdministrationService
LawEnforcement
ReligiousService
Buddhist
Catholic
Jewish
Muslim
Orthodox
Shop
Station
Tobacconist
TourismAgency

AirTransportation
RailTransportation
RoadTransportation
Bike
Bus
Taxi
Car
Motorcycle
Scooter
Skateboard
Van
WaterTransportation

% Entities schema

Agent
Organization
Club
Company
EducationalInstitution
Government
NonGovernmentalOrganization
ProfessionalAssociationUnion
Institution
PublicInstitution
SubGroup
Person
User
PersonUser
OrganizationUser
GroupUser

Artifact
Artwork
Handicraft
IndustrialWork
Component
Device

Collection
Family
Group
Series
Vocabulary
Corpus

ContentDescription
Category
Concept
Genre
Occupation
Period
Stuff
Subject
Trend
Language
Text
Sentence
Word

Document
Audio
Music
Noise
Song
Speech
Printable
Book
Booklet
Card
Cartography
DesignDiagram
Leaflet
Letter
Magazine
Manual
MusicalScore
Picture
Postcard
Poster
TextDocument
HandPrintedBook
Video
Documentary
Movie
Article
Visual
Microform
Projection

Event
Concert
Conference
Exhibition
Fair
HistoricalEvent
Lecture
Show

IntellectualWork
Algorithm
Approach
Discipline
Invention
ProgrammingLanguage
Standard
Regulation
Taxonomy
Technology
Theorem
TheoreticalModel
WorkOfArt
CartographicWork
Expression
MusicalWork

InternetComponent
Domain
EMailAddress
Protocol
URI
URL

Item

Nomen
Identifier

Place
Administrative
Continent
Country
Region/State
Province/County
Town
District
Road/Street/Square
Address
Building
Apartment
Castle
Cottage
House
Mansion
Skyscraper
Geographic
Area
Hill
Island
Lake
Land
Mountain
Peninsula
Planet
River
Sea
Valley
Beach
Basin
Butte
Canyon
Cave
Plateau

ProcessComponent
Activity
Case
Process
Task

TemporalSpecification
Duration
TimePeriod
Time
TimeInterval
Timeline
TimePoint
Year
Month
Day
DateTime

Citation

Edition

PointOfInterest
PlaceToVisit
Archive
Architecture
Church
Library
Monument
Museum
Palace
PanoramicView
Park
Ruin
Theater
AmusementPark
ArchaeologicalSite
Cathedral
Citadel
Fort
Fortress
Fountain
Landmark
Monastery
Mosque
Pagoda
Plaza
Shrine
Stadium
Temple
Tower
Wat
Service
Bank
EatingDrinking
Bar
Pub
Bistro
Cafe
Brasserie
Canteen
SnackBar
Grillroom
Stakehouse
Pizzeria
Tavern
TeaHouse
Market
Restaurant
GasStation
Hotel
HotelService
BikeTour
CookingClass
CulturalTour
DiscoParty
FacialTreatments
HappyHour
Yoga
Pilates
WalkingTour
Aerobics
AquaticExercise
BeautyService
BarberService
Bath
Jacuzzi
Tub
Massage
Eastern
Thai
Turkish
Shiatsu
Western
Swedish
Detox
Collagen
Firming
Pedicure
Sunbath
Shower
Swiss
Vichy
Steam
Roman
Air
Spa
Medical
Resort
Therapy
Aroma
HealthService
Hospital
Pharmacy
PostOffice
PublicService
AdministrationService
LawEnforcement
ReligiousService
Buddhist
Catholic
Jewish
Muslim
Orthodox
Shop
Station
Tobacconist
TourismAgency

Attraction

MeansOfTransportation
AirTransportation
RailTransportation
RoadTransportation
Bike
Bus
Taxi
Car
Motorcycle
Scooter
Skateboard
Van
WaterTransportation

Visit


% RELATIONSHIPS

% Relationships hierarchy

% Relationships schema

aliasOf
aliasOf

attributeOf
hasAttribute

belongsTo
includes

causes
causedBy

classifiedAs
hasClassification

clones
clonedBy

concerns
citedIn

connectedTo
connectedTo

describes
describedBy

developed
developedBy

entails
entailedBy

evaluated
evaluatedBy

evolves
evolvedBy

expresses
expressedBy

instanceOf
hasInstance

interactedWith
interactedWith

isA
hasSubclass

knows
knownBy

modified
modifiedBy

nextTo
nextTo

owned
ownedBy

partOf
hasPart

pertainsTo
pertainedBy

precedes
succeeds

produced
producedBy

relatedTo
relatedTo

relevantFor
pertains

requires
requiredBy

seeAlso
seeAlso

similarMeaningAs
similarMeaningAs

similarTo
similarTo

used
usedBy

wasIn
hosted

acquired
acquiredBy

aggregated
aggregatedBy

adopts
adoptedBy

available
availabilityOf

branchOf
controls

cites
citedIn

contributorOf
hasContributor

derivesFrom
spawned

hasAlternate
isAlternateOf

influenced
influencedBy

packagedWith
hadInPackage

references
referencedBy

updated
updatingOf

unavailable
unavailabilityOf

valid
validityOf

makes
madeBy


% GENERIC RULES

%% is_subclass(?Subclass, ?Superclass)
%
% Predicate to check if Subclass is a subclass at any hierarchy level of Superclass.
% This will work for any GraphBrain type, let it be entities, relationships or inverse relationships
 
is_subclass(Subclass, Superclass) :-
 	is_subclass_normal(Subclass, Superclass).
 
is_subclass(Subclass, Superclass) :-
 	\+ is_subclass_normal(Subclass, _),
 	is_subclass_inverse(Subclass, Superclass).
 
 
% is_subclass_normal(?Subclass, ?Superclass)
%
% Predicate to check if Subclass is a subclass at any hierarchy level of Superclass.
% This won't work for inverse relationships
 
is_subclass_normal(Subclass, Superclass) :-
 	subclass_of(Subclass, Superclass).
 
is_subclass_normal(Subclass, Superclass) :-
 	subclass_of(Subclass, Middleclass),
 	is_subclass_normal(Middleclass, Superclass).
 
 
% is_subclass_inverse(?SubclassInverse, ?SuperclassInverse)
%
% Predicate to check if SubclassInverse is a subclass at any hierarchy level of SuperclassInverse.
% This will ONLY work for inverse relationships, and it is typically used to retrieve all  
% parents/children of an inverse relationship
 
is_subclass_inverse(SubclassInverse, SuperclassInverse) :-
 	invert_relationship(SubclassInverse, SubclassClause),
 	SubclassClause =.. [SubclassName|_],
 	is_subclass_normal(SubclassName, SuperclassName),
 	invert_relationship(SuperclassName, SuperclassClauseInverse),
 	SuperclassClauseInverse =.. [SuperclassInverse|_].


%% invert_relationship(+RelationshipName, -InvertedRelationshipClause)
%
% Predicate which, given a RelationshipName returns its inverse relationship clause.
% This predicate is reflexive (i.e. given an inverted relationship name, the output will be the original
% relationship clause).
% If the relationship has no inverse, the output clause is the clause of the input relationship itself.
 
% we are inverting a "normal" relationship
invert_relationship(RelationshipName, InvertedRelationshipClause) :-
 	inverse_of(InvertedRelationshipName, RelationshipName),
 	!,
 	RelationshipToInvert =.. [RelationshipName, SubjObjList, AttributeList],
 	call(RelationshipToInvert),
 	invert_subj_obj(SubjObjList, InvertedSubjObjList),
 	InvertedRelationshipClause =.. [InvertedRelationshipName, InvertedSubjObjList, AttributeList].
 
% we are inverting an "inverted" relationship
invert_relationship(InvertedRelationshipName, RelationshipClause) :-
 	inverse_of(InvertedRelationshipName, RelationshipName),
 	RelationshipClause =.. [RelationshipName, _SubjObjList, _Attributes],
 	call(RelationshipClause),
 	!.
 
% we are inverting a relationship with no inverse
invert_relationship(RelationshipName, Relationship) :-
 	Relationship =.. [RelationshipName, _SubjObjList, _AttributeList],
 	call(Relationship).
 
 
% invert_subj_obj(+SubjectObjectList, -ObjectSubjectList)
%
% Predicate which inverts all references of a relationship
 
invert_subj_obj([], []).
 
invert_subj_obj([Subject-Object|T1], [Object-Subject|T2]) :-
 	invert_subj_obj(T1, T2).


%% gather_attributes(+SubC, -Attributes)
%
% Predicate which will gather all attributes of SubC + all attributes of its parents classes in its
% taxonomy. The order of the attributes is based on the "distance" of the parent class from SubC in its
% hierarchy: the attributes of the furthest parent class will be shown first.
 
% for entities
gather_attributes(SubC, Attributes) :-
 	Clause =.. [SubC, SubCAttributes],
 	call(Clause),
 	!,
 	findall(SuperC, is_subclass(SubC, SuperC), ParentsList),
 	gather_parent_attributes(ParentsList, ParentAttributes),
 	reverse(ParentAttributes, ReversedParentAttributes),
 	flatten([ReversedParentAttributes|SubCAttributes], Attributes).
 
% for relationships
gather_attributes(SubC, Attributes) :-
 	Clause =.. [SubC, _SubCReferences, SubCAttributes],
 	call(Clause),
 	!,
 	findall(SuperC, is_subclass(SubC, SuperC), ParentList),
 	gather_parent_attributes(ParentList, ParentAttributes),
 	reverse(ParentAttributes, ReversedParentAttributes),
 	flatten([ReversedParentAttributes|SubCAttributes], Attributes).

gather_attributes(InverseSubC, Attributes) :-
 	inverse_of(InverseSubC, SubC),
 	gather_attributes(SubC, Attributes).
 
 
% gather_parent_attributes(+ParentClasses, -ParentAttributes)
%
% Predicate which will gather, for each class in the ParentClasses argument, all of their attributes.
% The order of the attributes depends on the ParentClasses ordering
 
gather_parent_attributes([], []).
 
% for entities
gather_parent_attributes([ImmediateParentC|ParentClasses], [ImmediateParentCAttributes|ParentAttributes]) :-
 	Clause =.. [ImmediateParentC, ImmediateParentCAttributes],
 	call(Clause),
 	!,
 	gather_parent_attributes(ParentClasses, ParentAttributes).
 
% for relationships
gather_parent_attributes([ImmediateParentC|ParentClasses], [ImmediateParentCAttributes|ParentAttributes]) :-
 	Clause =.. [ImmediateParentC, _ParentCReferences, ImmediateParentCAttributes],
 	call(Clause),
 	gather_parent_attributes(ParentClasses, ParentAttributes).


%% gather_references(+SubC, -References)
%
% Predicate which will gather all references of SubC + all references of its parents classes in its
% taxonomy. The order of the references is based on the "distance" of the parent class from SubC in its
% hierarchy: the references of the furthest parent class will be shown first.
 
gather_references(SubC, References) :-
 	Clause =.. [SubC, SubCReferences, _SubCAttributes],
 	call(Clause),
 	!,
 	findall(SuperC, is_subclass(SubC, SuperC), ParentsList),
 	gather_parent_references(ParentsList, ParentReferences),
 	reverse(ParentReferences, ReversedParentReferences),
 	flatten([ReversedParentReferences|SubCReferences], References).

gather_references(InverseSubC, InvertedReferences) :-
 	inverse_of(InverseSubC, SubC),
 	gather_references(SubC, References),
 	invert_subj_obj(References, InvertedReferences).
 
 
% gather_parent_reference(+ParentClasses, -ParentReferences)
%
% Predicate which will gather, for each class in the ParentClasses argument, all of their references.
% The order of the references depends on the ParentClasses ordering
 
gather_parent_references([], []).
 
gather_parent_references([ImmediateParentC|ParentClasses], [ImmediateParentCReferences|ParentCReferences]) :-
 	Clause =.. [ImmediateParentC, ImmediateParentCReferences, _],
 	call(Clause),
 	gather_parent_references(ParentClasses, ParentCReferences).
