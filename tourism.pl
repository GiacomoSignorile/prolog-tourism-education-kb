:- use_module(library(lists)).
:- discontiguous inverse_of/2.
:- discontiguous subclass_of/2.
:- discontiguous entity/3.
:- discontiguous rel/4.
:- discontiguous place_info/5.
:- discontiguous person_info/2. % Corrected arity from your example (was 4, but data used 2 + ID)
:- discontiguous category_info/2.
:- discontiguous collection_info/2.
:- discontiguous event_info/3.

% --- SCHEMA DEFINITIONS ---

% -- Entities Hierarchy --
subclass_of('AirTransportation', 'MeansOfTransportation').
subclass_of('RailTransportation', 'MeansOfTransportation').
subclass_of('RoadTransportation', 'MeansOfTransportation').
subclass_of('Bike', 'RoadTransportation').
subclass_of('Bus', 'RoadTransportation').
subclass_of('Taxi', 'RoadTransportation').
subclass_of('Car', 'RoadTransportation').
subclass_of('Motorcycle', 'RoadTransportation').
subclass_of('Scooter', 'RoadTransportation').
subclass_of('Skateboard', 'RoadTransportation').
subclass_of('Van', 'RoadTransportation').
subclass_of('WaterTransportation', 'MeansOfTransportation').
subclass_of('Service', 'PointOfInterest').
subclass_of('Bank', 'Service').
subclass_of('EatingDrinking', 'Service').
subclass_of('Bar', 'EatingDrinking').
subclass_of('Pub', 'EatingDrinking').
subclass_of('Bistro', 'EatingDrinking').
subclass_of('Cafe', 'EatingDrinking').
subclass_of('Brasserie', 'EatingDrinking').
subclass_of('Canteen', 'EatingDrinking').
subclass_of('SnackBar', 'EatingDrinking').
subclass_of('Grillroom', 'EatingDrinking').
subclass_of('Stakehouse', 'EatingDrinking').
subclass_of('Pizzeria', 'EatingDrinking').
subclass_of('Tavern', 'EatingDrinking').
subclass_of('TeaHouse', 'EatingDrinking').
subclass_of('Market', 'EatingDrinking').
subclass_of('Restaurant', 'EatingDrinking').
subclass_of('GasStation', 'Service').
subclass_of('Hotel', 'Service').
subclass_of('HotelService', 'Service').
subclass_of('BikeTour', 'HotelService').
subclass_of('CookingClass', 'HotelService').
subclass_of('CulturalTour', 'HotelService').
subclass_of('DiscoParty', 'HotelService').
subclass_of('FacialTreatments', 'HotelService').
subclass_of('HappyHour', 'HotelService').
subclass_of('Yoga', 'HotelService').
subclass_of('Pilates', 'HotelService').
subclass_of('WalkingTour', 'HotelService').
subclass_of('Aerobics', 'HotelService').
subclass_of('AquaticExercise', 'HotelService').
subclass_of('BeautyService', 'Service').
subclass_of('BarberService', 'BeautyService').
subclass_of('Bath', 'BeautyService').
subclass_of('Jacuzzi', 'Bath').
subclass_of('Tub', 'Bath').
subclass_of('Massage', 'BeautyService').
subclass_of('Eastern', 'Massage').
subclass_of('Thai', 'Eastern').
subclass_of('Turkish', 'Eastern').
subclass_of('Shiatsu', 'Eastern').
subclass_of('Western', 'Massage').
subclass_of('Swedish', 'Western').
subclass_of('Detox', 'Western').
subclass_of('Collagen', 'Western').
subclass_of('Firming', 'Western').
subclass_of('Pedicure', 'BeautyService').
subclass_of('Sunbath', 'BeautyService').
subclass_of('Shower', 'BeautyService').
subclass_of('Swiss', 'Shower').
subclass_of('Vichy', 'Shower').
subclass_of('Steam', 'Shower').
subclass_of('Roman', 'Shower').
subclass_of('Air', 'Shower').
subclass_of('Spa', 'BeautyService').
subclass_of('Day', 'Spa').
subclass_of('Medical', 'Spa').
subclass_of('Resort', 'Spa').
subclass_of('Therapy', 'BeautyService').
subclass_of('Aroma', 'Therapy').
subclass_of('HealthService', 'Service').
subclass_of('Hospital', 'HealthService').
subclass_of('Pharmacy', 'HealthService').
subclass_of('PostOffice', 'Service').
subclass_of('PublicService', 'Service').
subclass_of('AdministrationService', 'PublicService').
subclass_of('LawEnforcement', 'PublicService').
subclass_of('ReligiousService', 'Service').
subclass_of('Buddhist', 'ReligiousService').
subclass_of('Catholic', 'ReligiousService').
subclass_of('Jewish', 'ReligiousService').
subclass_of('Muslim', 'ReligiousService').
subclass_of('Orthodox', 'ReligiousService').
subclass_of('Shop', 'Service').
subclass_of('Station', 'Service').
subclass_of('Tobacconist', 'Service').
subclass_of('TourismAgency', 'Service').
subclass_of('PlaceToVisit', 'PointOfInterest').
subclass_of('Archive', 'PlaceToVisit').
subclass_of('Architecture', 'PlaceToVisit').
subclass_of('Church', 'PlaceToVisit').
subclass_of('Library', 'PlaceToVisit').
subclass_of('Monument', 'PlaceToVisit').
subclass_of('Museum', 'PlaceToVisit').
subclass_of('Palace', 'PlaceToVisit').
subclass_of('PanoramicView', 'PlaceToVisit').
subclass_of('Park', 'PlaceToVisit').
subclass_of('Ruin', 'PlaceToVisit').
subclass_of('Theater', 'PlaceToVisit').
subclass_of('AmusementPark', 'PlaceToVisit').
subclass_of('ArchaeologicalSite', 'PlaceToVisit').
subclass_of('Cathedral', 'PlaceToVisit').
subclass_of('Citadel', 'PlaceToVisit').
subclass_of('Fort', 'PlaceToVisit').
subclass_of('Fortress', 'PlaceToVisit').
subclass_of('Fountain', 'PlaceToVisit').
subclass_of('Landmark', 'PlaceToVisit').
subclass_of('Monastery', 'PlaceToVisit').
subclass_of('Mosque', 'PlaceToVisit').
subclass_of('Pagoda', 'PlaceToVisit').
subclass_of('Plaza', 'PlaceToVisit').
subclass_of('Shrine', 'PlaceToVisit').
subclass_of('Stadium', 'PlaceToVisit').
subclass_of('Temple', 'PlaceToVisit').
subclass_of('Tower', 'PlaceToVisit').
subclass_of('Wat', 'PlaceToVisit').

% Top-level entities (assuming 'Entity' is the implicit root from DomainData)
% These are implicit if not specified, but good to have if your tool uses them.
% If 'Entity' is a defined type they inherit from, that's fine.
% For now, the rules work by checking subtypes of PointOfInterest, MeansOfTransportation etc.
% If you have a top-level 'Entity', you can add:
% subclass_of('Attraction', 'Entity').
% subclass_of('MeansOfTransportation', 'Entity').
% subclass_of('PointOfInterest', 'Entity').
% subclass_of('Visit', 'Entity').
% ... and for imported entities like Person, Place etc. if they are also direct subs of 'Entity'.

% -- Entity Attribute Schemas --
% These facts define the *direct* attributes of each entity type.
% The gather_attributes/2 rule will collect these and inherited ones.
'Attraction'([date, technique, material]).
'MeansOfTransportation'([availability, consumption, typeOfFuel]).
'AirTransportation'([transportType]).
'RailTransportation'([transportType]).
'RoadTransportation'([]).
'Bike'([]).
'Bus'([]).
'Taxi'([]).
'Car'([type]).
'Motorcycle'([]).
'Scooter'([]).
'Skateboard'([]).
'Van'([]).
'WaterTransportation'([transportType]).
'PointOfInterest'([place, address, phone, estimatedCost]). % Note: 'name' is not listed here, but used in facts. Add if it's a formal attribute.
'Service'([]).
'Bank'([]).
'EatingDrinking'([timeLimit, requiresTicket]).
'Bar'([]).
'Pub'([]).
'Bistro'([]).
'Cafe'([]).
'Brasserie'([]).
'Canteen'([]).
'SnackBar'([]).
'Grillroom'([]).
'Stakehouse'([]).
'Pizzeria'([]).
'Tavern'([]).
'TeaHouse'([]).
'Market'([type]).
'Restaurant'([type, foodType]).
'GasStation'([]).
'Hotel'([stars]).
'HotelService'([]).
'BikeTour'([]).
'CookingClass'([]).
'CulturalTour'([]).
'DiscoParty'([]).
'FacialTreatments'([]).
'HappyHour'([]).
'Yoga'([]).
'Pilates'([]).
'WalkingTour'([]).
'Aerobics'([]).
'AquaticExercise'([]).
'BeautyService'([]).
'BarberService'([]).
'Bath'([]).
'Jacuzzi'([]).
'Tub'([]).
'Massage'([]).
'Eastern'([]).
'Thai'([]).
'Turkish'([]).
'Shiatsu'([]).
'Western'([]).
'Swedish'([]).
'Detox'([]).
'Collagen'([]).
'Firming'([]).
'Pedicure'([]).
'Sunbath'([]).
'Shower'([]).
'Swiss'([]).
'Vichy'([]).
'Steam'([]).
'Roman'([]).
'Air'([]).
'Spa'([]).
'Day'([]).
'Medical'([]).
'Resort'([]).
'Therapy'([]).
'Aroma'([]).
'HealthService'([]).
'Hospital'([]).
'Pharmacy'([]).
'PostOffice'([]).
'PublicService'([]).
'AdministrationService'([type]).
'LawEnforcement'([type]).
'ReligiousService'([]).
'Buddhist'([]).
'Catholic'([]).
'Jewish'([]).
'Muslim'([]).
'Orthodox'([]).
'Shop'([type]).
'Station'([kind]).
'Tobacconist'([]).
'TourismAgency'([]).
'PlaceToVisit'([timeLimit, requiresTicket]). % Note: 'name' is not listed here.
'Archive'([]).
'Architecture'([]).
'Church'([]).
'Library'([]).
'Monument'([]).
'Museum'([]). % Note: 'name' is not listed here.
'Palace'([]).
'PanoramicView'([]).
'Park'([]).
'Ruin'([]).
'Theater'([]).
'AmusementPark'([]).
'ArchaeologicalSite'([]).
'Cathedral'([]).
'Citadel'([]).
'Fort'([]).
'Fortress'([]).
'Fountain'([]).
'Landmark'([]).
'Monastery'([]).
'Mosque'([]).
'Pagoda'([]).
'Plaza'([]).
'Shrine'([]).
'Stadium'([]).
'Temple'([]).
'Tower'([]).
'Wat'([]).
'Visit'([startDate, endDate, budget]).
% For imported entities like Person, Place, Category, Collection, Event,
% if they have attributes defined in *this* schema, add them here.
% e.g., 'Person'([name, birthDate]). 'Place'([name, latitude, longitude]).

% -- Relationship Inverse Definitions --
inverse_of('includes', 'belongsTo').
inverse_of('developedBy', 'developed').
inverse_of('kindOf', 'isA'). % Or hasSubclass/isA depending on original meaning
inverse_of('madeBy', 'makes').
inverse_of('ownedBy', 'owned').
inverse_of('pertains', 'relevantFor').
inverse_of('hosted', 'wasIn').

% -- Relationship Signature Schemas --
% These define Subject-Object pairings and direct attributes for each relationship.
'belongsTo'(['Attraction'-'Collection', 'PointOfInterest'-'Category', 'PointOfInterest'-'Collection'], []).
'developed'(['Person'-'Attraction'], [role, order]).
'isA'(['Artifact'-'Attraction', 'Place'-'PointOfInterest'], []). % Subject-Object from GBS: Artifact isA Attraction, Place isA PointOfInterest
'makes'(['Collection'-'Visit', 'Collection'-'Event', 'Person'-'Visit'], [role]).
'owned'(['Organization'-'Attraction', 'Person'-'Attraction'], [quantity, startDate, endDate, public]).
'relevantFor'(['Attraction'-'Category', 'Category'-'Attraction', 'Collection'-'Category', 'PointOfInterest'-'Category', 'Person'-'PointOfInterest', 'Category'-'PointOfInterest'], [role]).
'wasIn'(['Attraction'-'Place', 'Attraction'-'PointOfInterest', 'Collection'-'PointOfInterest', 'Person'-'PointOfInterest', 'PointOfInterest'-'Place', 'Visit'-'Event', 'Visit'-'Place'], [reason, address, startDate, endDate]).


% --- GENERIC SCHEMA UTILITY RULES ---

% is_subclass(?Subclass, ?Superclass)
is_subclass(Class, Class) :- !. % Reflexivity: A class is a subclass of itself.
is_subclass(Subclass, Superclass) :-
  is_subclass_normal(Subclass, Superclass).
is_subclass(Subclass, Superclass) :-
  \+ is_subclass_normal(Subclass, _), % Ensure it's not a normal subclass of anything (or specific Superclass failed)
  current_predicate(inverse_of/2),    % Only if inverse relationships are defined
  (inverse_of(Subclass, _) ; inverse_of(_, Subclass)), % Check if Subclass is part of an inverse_of pair
  is_subclass_inverse(Subclass, Superclass).

is_subclass_normal(Subclass, Superclass) :-
  subclass_of(Subclass, Superclass).
is_subclass_normal(Subclass, Superclass) :-
  subclass_of(Subclass, Middleclass),
  is_subclass_normal(Middleclass, Superclass).

is_subclass_inverse(SubclassInverse, SuperclassInverse) :-
  invert_relationship(SubclassInverse, SubclassClause), % Get the schema of the non-inverted counterpart
  SubclassClause =.. [SubclassName|_],                 % Extract its name
  is_subclass_normal(SubclassName, SuperclassName),    % Check hierarchy of non-inverted names
  invert_relationship(SuperclassName, SuperclassClauseInverse), % Get schema of the Superclass's inverse
  SuperclassClauseInverse =.. [SuperclassInverse|_].        % Extract its name

% invert_relationship(+RelationshipName, -InvertedRelationshipClause)
% Returns the schema clause (Name, Refs, Attrs) of the inverse.
invert_relationship(RelationshipName, InvertedRelationshipClause) :-
  inverse_of(InvertedRelationshipName, RelationshipName), % RelationshipName is the "normal" one
  schema_relationship_definition(RelationshipName, SubjObjList, AttributeList), % Get its definition
  !,
  invert_subj_obj(SubjObjList, InvertedSubjObjList),
  InvertedRelationshipClause =.. [InvertedRelationshipName, InvertedSubjObjList, AttributeList].
invert_relationship(InvertedRelationshipName, RelationshipClause) :-
  inverse_of(InvertedRelationshipName, RelationshipName), % InvertedRelationshipName is the one we are inverting
  schema_relationship_definition(RelationshipName, SubjObjList, AttributeList), % Get definition of its "normal" base
  !,
  RelationshipClause =.. [RelationshipName, SubjObjList, AttributeList].
invert_relationship(RelationshipName, RelationshipClause) :- % No inverse defined
  \+ inverse_of(_, RelationshipName),
  \+ inverse_of(RelationshipName, _),
  schema_relationship_definition(RelationshipName, SubjObjList, AttributeList),
  RelationshipClause =.. [RelationshipName, SubjObjList, AttributeList].

% schema_relationship_definition(RelName, SubjObjList, AttrList)
% Helper to get the original schema definition of a relationship.
schema_relationship_definition(RelName, SubjObjList, AttrList) :-
    Clause =.. [RelName, SubjObjList, AttrList],
    current_predicate(RelName/2), % Check if the schema fact (e.g., 'belongsTo'/2) exists
    call(Clause).

invert_subj_obj([], []).
invert_subj_obj([Subject-Object|T1], [Object-Subject|T2]) :-
  invert_subj_obj(T1, T2).

% gather_attributes(+ClassName, -AllAttributes)
% Gathers attributes from ClassName and all its superclasses.
gather_attributes(ClassName, AllAttributes) :-
    ClassName =.. [NameAtom], % Ensure ClassName is an atom if it's passed as a term
    (   current_predicate(NameAtom/1) -> % It's an entity schema fact like 'Attraction'([...]).
        SchemaClause =.. [NameAtom, DirectAttributes],
        call(SchemaClause),
        findall(SuperC, is_subclass(NameAtom, SuperC), ParentsListWithSelf),
        remove_self(NameAtom, ParentsListWithSelf, ParentsList),
        gather_parent_attributes_entity(ParentsList, ParentAttributesLists),
        flatten([ParentAttributesLists, DirectAttributes], FlatAttributes),
        list_to_set(FlatAttributes, AllAttributes) % Remove duplicates, keep order of first appearance somewhat
    ;   current_predicate(NameAtom/2) -> % It's a relationship schema fact like 'belongsTo'([...],[...]).
        SchemaClause =.. [NameAtom, _References, DirectAttributes],
        call(SchemaClause),
        findall(SuperC, is_subclass(NameAtom, SuperC), ParentsListWithSelf),
        remove_self(NameAtom, ParentsListWithSelf, ParentsList),
        gather_parent_attributes_relationship(ParentsList, ParentAttributesLists),
        flatten([ParentAttributesLists, DirectAttributes], FlatAttributes),
        list_to_set(FlatAttributes, AllAttributes)
    ;   inverse_of(NameAtom, BaseRel) -> % It's an inverse relationship name
        gather_attributes(BaseRel, AllAttributes) % Inverse relationships share attributes with their base
    ;   AllAttributes = [] % Not a defined entity or relationship, or no attributes
    ).

remove_self(_, [], []).
remove_self(Self, [Self|T], T) :- !.
remove_self(Self, [H|T1], [H|T2]) :- remove_self(Self, T1, T2).

gather_parent_attributes_entity([], []).
gather_parent_attributes_entity([Parent|RestParents], AllParentAttributes) :-
    ParentSchema =.. [Parent, ParentDirectAttributes],
    (   call(ParentSchema) -> true ; ParentDirectAttributes = [] ),
    gather_parent_attributes_entity(RestParents, RestAttributes),
    append(ParentDirectAttributes, RestAttributes, AllParentAttributes).

gather_parent_attributes_relationship([], []).
gather_parent_attributes_relationship([Parent|RestParents], AllParentAttributes) :-
    ParentSchema =.. [Parent, _ParentRefs, ParentDirectAttributes],
    (   call(ParentSchema) -> true ; ParentDirectAttributes = [] ),
    gather_parent_attributes_relationship(RestParents, RestAttributes),
    append(ParentDirectAttributes, RestAttributes, AllParentAttributes).


% gather_references(+RelationshipName, -AllReferences)
% Gathers Subject-Object references from RelationshipName and all its super-relationships.
gather_references(RelationshipName, AllReferences) :-
    RelationshipName =.. [NameAtom],
    (   current_predicate(NameAtom/2) -> % It's a "normal" relationship schema fact
        SchemaClause =.. [NameAtom, DirectReferences, _Attributes],
        call(SchemaClause),
        findall(SuperR, is_subclass(NameAtom, SuperR), ParentsListWithSelf),
        remove_self(NameAtom, ParentsListWithSelf, ParentsList),
        gather_parent_references_relationship(ParentsList, ParentReferencesLists),
        flatten([ParentReferencesLists, DirectReferences], FlatReferences),
        list_to_set(FlatReferences, AllReferences)
    ;   inverse_of(NameAtom, BaseRel) -> % It's an inverse relationship name
        gather_references(BaseRel, BaseReferences),
        invert_subj_obj(BaseReferences, AllReferences) % Invert the references for the inverse
    ;   AllReferences = [] % Not a defined relationship or no references
    ).

gather_parent_references_relationship([], []).
gather_parent_references_relationship([Parent|RestParents], AllParentReferences) :-
    ParentSchema =.. [Parent, ParentDirectReferences, _ParentAttrs],
    (   call(ParentSchema) -> true ; ParentDirectReferences = [] ),
    gather_parent_references_relationship(RestParents, RestReferences),
    append(ParentDirectReferences, RestReferences, AllParentReferences).


% --- INSTANCE FACTS ---

% -- Place Info (PlaceID, Name, AddressString, Lat, Long) --
place_info(p1, 'Paris City Center', '1 Rue de Rivoli, 75001 Paris, France', 48.8566, 2.3522).
place_info(p2, 'Rome Historic Center', 'Piazza Navona, 00186 Rome, Italy', 41.8992, 12.4731).
place_info(p3, 'Kyoto Gion District', 'Gion, Higashiyama Ward, Kyoto, Japan', 35.0030, 135.7780).

% -- Person Info (PersonID, Name) --
person_info(person1, 'Alice Wonderland').
person_info(person2, 'Bob The Builder').
person_info(artist_davinci, 'Leonardo da Vinci').
% Note: Your discontiguous directive was person_info/4, facts are person_info/2.
% For consistency with facts, I'll assume person_info(ID, Name).
% If you need more attributes, define them in person_info/N and in entity schema for 'Person'.

% -- Category Info (CategoryID, Name) --
category_info(cat_art, 'Art').
category_info(cat_history, 'History').
category_info(cat_foodie, 'Foodie Experience').
category_info(cat_relax, 'Relaxation').
category_info(cat_architecture, 'Architecture').

% -- Collection Info (CollectionID, Name) --
collection_info(col1, 'Paris Must-See Art').
collection_info(col2, 'Roman Holiday Itinerary').

% -- Event Info (EventID, Name, Date) --
event_info(event_paris_fest, 'Paris Summer Festival', date(2024,7,14)).

% -- Entities (EntityID, EntityType, ListOfAttributes) --
% Added attr(name, ...) where it was missing in schema but present in facts, for consistency.
% Consider formally adding 'name' to 'PointOfInterest' and 'PlaceToVisit' schemas if it's a common attribute.
entity(louvre, 'Museum', [attr(name, 'Louvre Museum'), attr(place, p1), attr(estimatedCost, 20), attr(requiresTicket, true), attr(phone, '+33 1 40 20 50 50')]).
entity(eiffel_tower, 'Tower', [attr(name, 'Eiffel Tower'), attr(place, p1), attr(estimatedCost, 25), attr(requiresTicket, true), attr(timeLimit, 180)]).
entity(colosseum, 'ArchaeologicalSite', [attr(name, 'Colosseum'), attr(place, p2), attr(estimatedCost, 18), attr(requiresTicket, true)]).
entity(trevi_fountain, 'Fountain', [attr(name, 'Trevi Fountain'), attr(place, p2), attr(estimatedCost, 0), attr(requiresTicket, false)]).
entity(city_park_paris, 'Park', [attr(name, 'Jardin du Luxembourg'), attr(place, p1), attr(estimatedCost, 0), attr(requiresTicket, false)]).
entity(musee_orsay, 'Museum', [attr(name, 'Musee d Orsay'), attr(place, p1), attr(estimatedCost, 16)]).
entity(trattoria_romana, 'Restaurant', [attr(name, 'Trattoria Romana'), attr(place, p2), attr(foodType, 'Generic'), attr(type, 'Italian'), attr(estimatedCost, 40)]).
entity(le_bistro_parisien, 'Bistro', [attr(name, 'Le Bistro Parisien'), attr(place, p1), attr(estimatedCost, 60), attr(requiresTicket, false)]).
entity(kyoto_ramen, 'Restaurant', [attr(name, 'Kyoto Ramen Shop'), attr(place, p3), attr(foodType, 'Generic'), attr(type, 'Japanese'), attr(estimatedCost, 15)]).
entity(sushi_master_kyoto, 'Restaurant', [attr(name, 'Sushi Master Kyoto'), attr(place, p3), attr(foodType, 'Fish'), attr(type, 'Japanese High-End'), attr(estimatedCost, 150)]).

entity(grand_hotel_paris, 'Hotel', [attr(name, 'Grand Hotel Paris'), attr(place, p1), attr(stars, 5), attr(phone, '+33 1 23 45 67 89')]).
entity(budget_inn_rome, 'Hotel', [attr(name, 'Budget Inn Rome'), attr(place, p2), attr(stars, 2)]).
entity(ryokan_kyoto, 'Hotel', [attr(name, 'Gion Ryokan'), attr(place, p3), attr(stars, 4)]).

entity(ghp_yoga_class, 'Yoga', [attr(name, 'Sunrise Yoga')]).
entity(ghp_spa_service, 'Resort', [attr(name, 'Hotel Main Spa')]).
entity(ghp_cooking_class, 'CookingClass', [attr(name, 'French Pastry Class')]).
entity(ghp_bike_tour, 'BikeTour', [attr(name, 'Paris City Bike Tour')]).
entity(ryokan_onsen, 'Spa', [attr(name, 'Private Onsen Experience')]).
entity(ryokan_tea_ceremony, 'HotelService', [attr(name, 'Traditional Tea Ceremony')]).
entity(ghp_spa_service, 'Resort', [attr(name, 'Hotel Main Spa')]).
entity(ryokan_onsen, 'Spa', [attr(name, 'Private Onsen Experience')]).

entity(paris_main_station, 'Station', [attr(name, 'Gare du Nord'), attr(place, p1), attr(kind, 'Train')]).
entity(rome_fco_airport, 'Station', [attr(name, 'Fiumicino Airport'), attr(place, p2), attr(kind, 'Airport')]).

entity(my_sedan, 'Car', [attr(name, 'My Sedan'), attr(type, 'Sedan'), attr(availability, true), attr(consumption, '7L/100km'), attr(typeOfFuel, 'Gasoline')]).
entity(city_bus_101, 'Bus', [attr(name, 'City Bus 101'), attr(availability, true), attr(consumption, '20L/100km'), attr(typeOfFuel, 'Diesel')]).
entity(paris_metro_line1, 'RailTransportation', [attr(name, 'Paris Metro Line 1'), attr(transportType, 'Subway'), attr(availability, true)]).

entity(mona_lisa, 'Attraction', [attr(name, 'Mona Lisa'), attr(date, '1503-1506'), attr(technique, 'Oil on poplar panel'), attr(material, 'Oil paint, Poplar wood')]).
entity(david_statue, 'Attraction', [attr(name, 'Statue of David'), attr(date, '1501-1504'), attr(technique, 'Sculpture'), attr(material, 'Marble')]).

entity(paris_trip_alice, 'Visit', [attr(name, 'Alice Paris Trip'), attr(startDate, date(2024,8,1)), attr(endDate, date(2024,8,7)), attr(budget, 2000)]).
entity(rome_weekend_bob, 'Visit', [attr(name, 'Bob Rome Weekend'), attr(startDate, date(2024,9,10)), attr(endDate, date(2024,9,12)), attr(budget, 500)]).

entity(pharmacy_central_paris, 'Pharmacy', [attr(name, 'Central Pharmacy Paris'), attr(place, p1), attr(phone, '+33 1 98 76 54 32')]).
entity(bank_of_paris, 'Bank', [attr(name, 'Bank of Paris'), attr(place, p1)]).
entity(bike1, 'Bike', []).
entity(civic, 'Car', [attr(name, 'Civic'), attr(type, 'Sedan'), attr(availability, true), attr(consumption, '6L/100km'), attr(typeOfFuel, 'Electric')]).

% Free museums in Paris
entity(museum_modern_art, 'Museum', [attr(name, 'Museum of Modern Art'), attr(place, p1), attr(estimatedCost, 0), attr(requiresTicket, false)]).
entity(museum_nature_paris, 'Museum', [attr(name, 'Natural History Museum'), attr(place, p1), attr(estimatedCost, 0), attr(requiresTicket, false)]).

% Free museums in Rome
entity(museum_roman_history, 'Museum', [attr(name, 'Museum of Roman History'), attr(place, p2), attr(estimatedCost, 0), attr(requiresTicket, false)]).

% Free museums in Kyoto
entity(museum_kyoto_art, 'Museum', [attr(name, 'Kyoto Art Museum'), attr(place, p3), attr(estimatedCost, 0), attr(requiresTicket, false)]).
% -- Relationships (RelationshipName, SubjectID, ObjectID, ListOfAttributes) --
rel(wasIn, mona_lisa, louvre, []).
rel(wasIn, louvre, p1, []).
rel(wasIn, eiffel_tower, p1, []).
rel(wasIn, colosseum, p2, []).
rel(wasIn, grand_hotel_paris, p1, []).
rel(wasIn, budget_inn_rome, p2, []).
rel(wasIn, ryokan_kyoto, p3, []).
rel(wasIn, trattoria_romana, p2, []).
rel(wasIn, le_bistro_parisien, p1, []).
rel(wasIn, kyoto_ramen, p3, []).
rel(wasIn, sushi_master_kyoto, p3, []).
rel(wasIn, city_park_paris, p1, []).
rel(wasIn, trevi_fountain, p2, []).
rel(wasIn, pharmacy_central_paris, p1, []).
rel(wasIn, bank_of_paris, p1, []).


rel(wasIn, ghp_yoga_class, grand_hotel_paris, [attr(reason, 'hotel_amenity')]).
rel(wasIn, ghp_spa_service, grand_hotel_paris, [attr(reason, 'hotel_amenity')]).
rel(wasIn, ghp_cooking_class, grand_hotel_paris, [attr(reason, 'hotel_activity')]).
rel(wasIn, ghp_bike_tour, grand_hotel_paris, [attr(reason, 'hotel_activity')]).
rel(wasIn, ryokan_onsen, ryokan_kyoto, [attr(reason, 'hotel_amenity')]).
rel(wasIn, ryokan_tea_ceremony, ryokan_kyoto, [attr(reason, 'cultural_offering')]).

rel(relevantFor, louvre, cat_art, [attr(role, 'primary_focus')]).
rel(relevantFor, louvre, cat_architecture, [attr(role, 'building_style')]).
rel(relevantFor, colosseum, cat_history, [attr(role, 'historical_significance')]).
rel(relevantFor, colosseum, cat_architecture, [attr(role, 'roman_architecture')]).
rel(relevantFor, trattoria_romana, cat_foodie, [attr(role, 'dining_experience')]).
rel(relevantFor, ghp_spa_service, cat_relax, [attr(role, 'wellness_service')]).
rel(relevantFor, ryokan_onsen, cat_relax, [attr(role, 'wellness_service')]).

rel(belongsTo, louvre, col1, []).
rel(belongsTo, eiffel_tower, col1, []).
rel(belongsTo, colosseum, col2, []).
rel(belongsTo, trevi_fountain, col2, []).

rel(makes, col1, paris_trip_alice, [attr(role, 'itinerary_basis')]).
rel(makes, person1, paris_trip_alice, [attr(role, 'planner')]).
rel(makes, person2, rome_weekend_bob, [attr(role, 'traveler')]).
rel(makes, col2, rome_weekend_bob, []).
rel(developed, artist_davinci, mona_lisa, [attr(role, 'painter')]).

% rel(isA, p1, louvre, []). % This interpretation of isA(Place, POI) is tricky.
                           % The schema is 'Place'-'PointOfInterest'.
                           % It implies a specific Place instance *is* a POI.
                           % E.g., entity(louvre_building, 'Place', [...]).
                           %        entity(louvre_poi, 'Museum', [...]).
                           %        rel(isA, louvre_building, louvre_poi, []).
                           % For now, focusing on isA(Artifact, Attraction) if you have Artifact entities.
                           % entity(mona_lisa_artifact_obj, 'Artifact', [attr(description, 'The physical painting')]).
                           % rel(isA, mona_lisa_artifact_obj, mona_lisa, []).


% --- DOMAIN-SPECIFIC RULES (30) ---

% Helper to get a single attribute value from an entity's attribute list
get_attribute_value(EntityID, AttrName, Value) :-
    entity(EntityID, _, Attributes),
    member(attr(AttrName, Value), Attributes).
get_attribute_value(EntityID, AttrName, default) :- % Default if attribute not found
    entity(EntityID, _, Attributes),
    \+ member(attr(AttrName, _), Attributes).

% 1. Identify any Point of Interest
is_poi(PoiID) :-
    entity(PoiID, Type, _),
    is_subclass(Type, 'PointOfInterest').

% 2. Identify any eating/drinking place
is_eating_place(PlaceID) :-
    entity(PlaceID, Type, _),
    is_subclass(Type, 'EatingDrinking').

% 3. Find restaurants of a specific food type
restaurant_food_type(RestaurantID, FoodType) :-
    entity(RestaurantID, 'Restaurant', Attributes), % Assumes direct type 'Restaurant'
    member(attr(foodType, FoodType), Attributes).

% General rule to find hotels based on star rating and an operator
% Operator can be '>=', '=<', '=', '<', '>'
hotel_with_star_rating(HotelID, Operator, Value) :-
    entity(HotelID, 'Hotel', Attributes),       
    member(attr(stars, ActualStars), Attributes), 
    number(ActualStars),                        
    number(Value),                             
    ComparisonGoal =.. [Operator, ActualStars, Value],
    call(ComparisonGoal).

% 4. Find luxury hotels (e.g., 4 stars or more)
% Redefined using the general rule
is_luxury_hotel(HotelID) :-
    hotel_with_star_rating(HotelID, >=, 4).

% 5. Find budget hotels (e.g., 2 stars or less)
% Redefined using the general rule
is_budget_hotel(HotelID) :-
    hotel_with_star_rating(HotelID, =<, 2). % Using 2 as per typical budget definition

% 6. Check if a POI requires a ticket
poi_requires_ticket(PoiID) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, requiresTicket, true).

% 7. Check if a POI has free entry
poi_is_free(PoiID) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, requiresTicket, false).
poi_is_free(PoiID) :- % Also consider free if cost is 0 and requiresTicket is not explicitly false
    is_poi(PoiID),
    \+ get_attribute_value(PoiID, requiresTicket, true), % Not explicitly true
    get_attribute_value(PoiID, estimatedCost, Cost),
    Cost =:= 0.

% 8. Find POIs with an estimated cost below a certain budget
poi_cheaper_than(PoiID, MaxCost) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, estimatedCost, Cost),
    number(Cost), % Ensure Cost is a number
    Cost < MaxCost.

% Defining types that, when found in a hotel, are hotel services 
is_typical_hotel_amenity_type(Type) :-
    member(Type, ['Spa', 'Resort', 'Yoga', 'Pilates', 'BikeTour', 'Restaurant', 'Bar']).

% 9. Find hotels offering a specific type of hotel service (e.g., 'Yoga', 'Spa')
hotel_offers_service_type(HotelID, ServiceEntityID, TargetServiceType) :-
    entity(HotelID, 'Hotel', _),
    rel(wasIn, ServiceEntityID, HotelID, _),
    entity(ServiceEntityID, ActualServiceEntityType, _),
    (   is_subclass(ActualServiceEntityType, 'HotelService') % Formal check
    ;   is_typical_hotel_amenity_type(ActualServiceEntityType) % Practical check
    ),
    is_subclass(ActualServiceEntityType, TargetServiceType).

% Find all services offered by a luxury hotel
luxury_hotel_all_services(LuxuryHotelID, ListOfServiceIDs) :-
    is_luxury_hotel(LuxuryHotelID), % Use existing rule
    findall(ServiceID,
            hotel_offers_service_type(LuxuryHotelID, ServiceID, _AnyServiceType), % _AnyServiceType as wildcard
            ListOfServiceIDs).

% 10. Find POIs located at a specific place (using PlaceID from entity attribute)
poi_at_place(PoiID, PlaceID) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, place, PlaceID).

% 11. Find POIs in a specific city (by matching PlaceID to place_info name)
poi_in_city(PoiID, CityName) :-
    poi_at_place(PoiID, PlaceID), % Uses rule 10
    place_info(PlaceID, CityName, _, _, _).

% 12. Generalize: is something a "cultural site"?
is_cultural_site(SiteID) :-
    entity(SiteID, Type, _),
    ( is_subclass(Type, 'Museum');
      is_subclass(Type, 'ArchaeologicalSite');
      is_subclass(Type, 'Monument');
      is_subclass(Type, 'Palace');
      is_subclass(Type, 'Theater');
      is_subclass(Type, 'Church'); % Includes Cathedrals, Mosques etc. if they are Churches
      is_subclass(Type, 'Cathedral');
      is_subclass(Type, 'Library');
      is_subclass(Type, 'Archive')
    ).

% 13. Find relaxation spots (Spas, Parks, some types of HotelServices like Yoga) generalized with a list.
relaxation_type(Type) :- member(Type, ['Spa', 'Park', 'Yoga', 'Pilates', 'Resort', 'Massage']).
is_relaxation_spot(ID) :-
  entity(ID, T, _), relaxation_type(Base),
  is_subclass(T, Base).


% 14. Is a transportation means a road vehicle?
is_road_vehicle(VehicleID) :-
    entity(VehicleID, Type, _),
    is_subclass(Type, 'RoadTransportation').

% 15. Is a transportation means eco-friendly (e.g. Bike, Skateboard)?
is_eco_friendly_transport(TransportID) :-
    entity(TransportID, Type, _),
    ( is_subclass(Type, 'Bike') ;
      is_subclass(Type, 'Skateboard');
      get_attribute_value(TransportID, typeOfFuel, 'Electric')
    ).

% Find POIs for a specific category and count them
count_pois_for_category(CategoryName, Count) :-
    category_info(_CatID, CategoryName), % Validate CategoryName and get CatID if needed
    findall(PoiID, poi_for_category(PoiID, CategoryName), PoiList),
    length(PoiList, Count).

% 16. Find POIs relevant for a specific category (e.g., 'Art')
poi_for_category(PoiID, CategoryName) :-
    is_poi(PoiID),
    category_info(CatID, CategoryName),
    rel(relevantFor, PoiID, CatID, _).

% 17. Find a person who planned a specific visit
visit_planner(VisitID, PersonID) :-
    entity(VisitID, 'Visit', _),
    person_info(PersonID, _Name),
    rel(makes, PersonID, VisitID, Attributes),
    member(attr(role, 'planner'), Attributes).

% 18. Find all POIs included in a person's visit (via collections linked to the visit)
poi_in_person_visit(PersonID, PoiID) :-
    person_info(PersonID, _),
    rel(makes, PersonID, VisitID, _), % Person makes/plans a visit
    entity(VisitID, 'Visit', _),
    rel(makes, CollectionID, VisitID, _), % Collection makes up the visit
    collection_info(CollectionID, _),
    rel(belongsTo, PoiID, CollectionID, _), % POI belongs to that collection
    is_poi(PoiID).

% 19. Is an attraction made of a specific material? (allows partial match)
attraction_material(AttractionID, MaterialString) :-
    entity(AttractionID, 'Attraction', Attributes),
    member(attr(material, ActualMaterial), Attributes),
    (   ground(MaterialString) -> 
        sub_string(ActualMaterial, _, _, _, MaterialString)
    ;   
        MaterialString = ActualMaterial
    ).

% 20. Find attractions developed by a specific person (e.g. artist)
attraction_by_developer(AttractionID, PersonID) :-
    entity(AttractionID, 'Attraction', _),
    person_info(PersonID, _Name),
    rel(developed, PersonID, AttractionID, _).

% 21. Is a POI a type of 'Service'?
is_service_poi(PoiID) :-
    entity(PoiID, Type, _),
    is_subclass(Type, 'Service').

% 22. Is a POI a type of 'PlaceToVisit'?
is_place_to_visit_poi(PoiID) :-
    entity(PoiID, Type, _),
    is_subclass(Type, 'PlaceToVisit').

% 23. Find hotels that offer any kind of beauty service
hotel_offers_beauty_service(HotelID, ServiceEntityID, SpecificBeautyServiceType) :-
    hotel_offers_service_type(HotelID, ServiceEntityID, _AnyHotelServiceType), % Finds any service in hotel
    entity(ServiceEntityID, SpecificBeautyServiceType, _), % Gets the specific type of that service entity
    is_subclass(SpecificBeautyServiceType, 'BeautyService'). % Checks if it's a beauty service

% 24. Find places that offer Thai Massage
offers_thai_massage(PlaceID) :- % PlaceID could be a hotel or a standalone spa
    entity(PlaceID, 'Hotel', _), % If it's a hotel
    hotel_offers_service_type(PlaceID, _ServiceEntityID, 'Thai').
offers_thai_massage(PlaceID) :- % If it's a standalone BeautyService entity that is Thai
    entity(PlaceID, 'Thai', _), % Directly a 'Thai' massage service entity
    is_subclass('Thai', 'BeautyService'). % And it's a subtype of BeautyService

% 25. Find POIs that have a time limit for visits
poi_has_time_limit(PoiID, TimeLimitMinutes) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, timeLimit, TimeLimitMinutes),
    number(TimeLimitMinutes), TimeLimitMinutes > 0.

% 26. Find available road transportation
available_road_transport(TransportID, Type) :-
    entity(TransportID, Type, Attributes),
    is_subclass(Type, 'RoadTransportation'),
    member(attr(availability, true), Attributes).

% 27. Is an eating place a Pizzeria or a Stakehouse?
is_pizzeria_or_stakehouse(PlaceID) :-
    entity(PlaceID, Type, _),
    (is_subclass(Type, 'Pizzeria') ; is_subclass(Type, 'Stakehouse')).

% 28. Find POIs that are religious sites (based on ReligiousService hierarchy)
is_religious_service_site(SiteID, SpecificReligionType) :-
    entity(SiteID, SpecificReligionType, _),
    is_subclass(SpecificReligionType, 'ReligiousService').
% If you mean places of worship like Churches, Mosques that are 'PlaceToVisit':
is_place_of_worship(SiteID, Type) :-
    entity(SiteID, Type, _),
    ( is_subclass(Type, 'Church');
      is_subclass(Type, 'Mosque');
      is_subclass(Type, 'Temple');
      is_subclass(Type, 'Shrine');
      is_subclass(Type, 'Synagogue') % Assuming Synagogue would be under Jewish if defined
    ).


% 29. Find shops of a particular type (e.g., 'Gifts')
shop_of_type(ShopID, ShopType) :-
    entity(ShopID, 'Shop', Attributes), % Assumes direct type 'Shop'
    member(attr(type, ShopType), Attributes).

% 30. Find stations serving a specific kind of transport (e.g., 'Train', 'Airport')
station_serves_transport_kind(StationID, Kind) :-
    entity(StationID, 'Station', Attributes), % Assumes direct type 'Station'
    member(attr(kind, Kind), Attributes).

% 31. Calculate the average cost of museums in a city
average_museum_cost_in_city(CityName, AverageCost) :-
    poi_in_city(MuseumID, CityName),          % Find POIs in the city
    entity(MuseumID, 'Museum', _Attributes), % Check if it's a Museum
    % Collect all valid costs
    findall(Cost,
            (   poi_in_city(MID, CityName),
                entity(MID, 'Museum', _),
                get_attribute_value(MID, estimatedCost, Cost),
                number(Cost) % Ensure it's a number
            ),
            CostsList),
    CostsList \= [], % Ensure there are museums with costs
    sum_list(CostsList, TotalCost),
    length(CostsList, NumberOfMuseums),
    AverageCost is TotalCost / NumberOfMuseums.

% For unique museums to avoid issues if poi_in_city yields duplicates for some reason
average_museum_cost_in_city_unique(CityName, AverageCost) :-
    setof(Cost, MID^CityPoi^( % Existential quantification for variables not in Cost
                poi_in_city(MID, CityName),
                entity(MID, 'Museum', _),
                get_attribute_value(MID, estimatedCost, Cost),
                number(Cost)
            ), CostsList), % setof ensures unique costs if a museum had multiple cost entries (unlikely with current facts)
                           % or unique museum IDs if we did setof(MID, ...) then got costs.
                           % For this, let's assume unique museums from poi_in_city + entity check.
    CostsList \= [],
    sum_list(CostsList, TotalCost),
    length(CostsList, NumberOfMuseums),
    AverageCost is TotalCost / NumberOfMuseums.

% Main rule for finding POIs with custom filters
% Arguments:
% - PoiID: Output variable for the found Point of Interest ID.
% - DistrictFilterValue, TypeFilterValue, etc.: Values to filter by.
% - EN_District, EN_Type, etc.: Boolean flags (true/false) to enable/disable filters.

% Main rule for finding POIs with custom filters, revised to use existing helpers
% Arguments:
% - PoiID: Output variable for the found Point of Interest ID.
% - CityFilterValue, TypeFilterValue, etc.: Values to filter by.
% - EN_City, EN_Type, etc.: Boolean flags (true/false) to enable/disable filters.

poi_with_custom_filters(
    PoiID,
    CityFilterValue, EntityTypeFilterValue, % Changed District to City, Type to EntityType
    CategoryFilterValue, AgeFilterValue,    % Changed Activity to Category
    TimeLimitFilterValue, MaxCostFilterValue, EntranceFeeFilterValue, % Clarified names
    EN_City, EN_EntityType,
    EN_Category, EN_Age, % Renamed EN_ActivityAge
    EN_TimeLimit, EN_MaxCost, EN_EntranceFee % Clarified names
) :-
    % Start by finding any POI. Filters will narrow it down.
    is_poi(PoiID), % Ensures PoiID is a Point of Interest (Rule 1)

    % Apply City Filter (formerly District)
    ( EN_City -> poi_in_city(PoiID, CityFilterValue) % Uses Rule 11
    ; true
    ),

    % Apply Entity Type Filter
    ( EN_EntityType ->
        ( entity(PoiID, ActualType, _), % Get actual type of the POI
          is_subclass(ActualType, EntityTypeFilterValue) % Check if it's a subclass of the desired type
        )
    ; true
    ),

    % Apply Category Filter (formerly Activity)
    ( EN_Category -> poi_for_category(PoiID, CategoryFilterValue) % Uses Rule 16
    ; true
    ),

    % Apply Age Filter (Still a placeholder, as no direct age rule exists)
    ( EN_Age -> activity_for_age_placeholder(CategoryFilterValue, AgeFilterValue, PoiID)
    ; true
    ),

    % Apply Time Limit Filter (Interpreting TIME as a maximum visit duration)
    ( EN_TimeLimit ->
        ( poi_has_time_limit(PoiID, ActualTimeLimit), % Uses Rule 25
          ActualTimeLimit =< TimeLimitFilterValue % Check if actual limit is within desired max limit
        )
    ; true
    ),

    % Apply Max Cost Filter (Interpreting BUDGET as a maximum POI cost)
    ( EN_MaxCost -> poi_cheaper_than(PoiID, MaxCostFilterValue) % Uses Rule 8 (checks Cost < MaxCostFilterValue)
                                                                % If you need Cost =< MaxCost, use get_attribute_value and compare
    ; true
    ),
    % Alternative for Max Cost if poi_cheaper_than (which is strictly <) is not suitable:
    % ( EN_MaxCost ->
    %     ( get_attribute_value(PoiID, estimatedCost, Cost),
    %       ( Cost == default -> true ; % Or specific handling for default
    %         number(Cost) -> Cost =< MaxCostFilterValue
    %       )
    %     )
    % ; true
    % ),


    % Apply Entrance Fee Filter
    ( EN_EntranceFee ->
        ( (EntranceFeeFilterValue == free, poi_is_free(PoiID)) % Uses Rule 7
        ; (EntranceFeeFilterValue == paid, poi_requires_ticket(PoiID)) % Uses Rule 6
        ; (number(EntranceFeeFilterValue), % If it's a max numeric fee
             get_attribute_value(PoiID, estimatedCost, Cost),
             ( Cost == default -> true ; % If no cost, assume it fits (or could fail if strict)
               number(Cost) -> Cost =< EntranceFeeFilterValue
             )
          )
        ; (\+ member(EntranceFeeFilterValue, [free, paid]), \+ number(EntranceFeeFilterValue), true) % If value is not free/paid/number, ignore this specific sub-filter
        )
    ; true
    ).

% Placeholder for age filter if you were to develop it
activity_for_age_placeholder(_Category, _Age, _PoiID) :-
    % Logic to check if PoiID (related to _Category) is suitable for _Age
    % Example: entity(PoiID, _, Attributes), member(attr(minAge, Min), Attributes), Age >= Min.
    true. % Currently always true

% --- SEARCH RULES ---

% Helper: Get entity name if it exists, otherwise use its ID as name
get_entity_display_name(EntityID, DisplayName) :-
    get_attribute_value(EntityID, name, NameValue),
    NameValue \== default, % Check if 'name' attribute exists and has a value
    !,
    DisplayName = NameValue.
get_entity_display_name(EntityID, EntityID). % Fallback to ID if no name attribute

% S1: Find Point of Interest by exact name
find_poi_by_name(NameString, PoiID) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, name, NameString).

% S2: Find Point of Interest by partial name (case-insensitive for substring)
find_poi_by_partial_name(SearchString, PoiID) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, name, ActualName),
    string_lower(SearchString, LowerSearch),
    string_lower(ActualName, LowerActualName),
    sub_string(LowerActualName, _, _, _, LowerSearch).

% S3: Find entities of a specific type with a specific attribute value
find_entity_by_type_and_attribute(EntityType, AttrName, AttrValue, EntityID) :-
    entity(EntityID, ActualEntityType, Attributes),
    is_subclass(ActualEntityType, EntityType), % EntityType can be a superclass
    member(attr(AttrName, AttrValue), Attributes).

% Example usage of S3:
% ?- find_entity_by_type_and_attribute('Hotel', stars, 5, HotelID).
% ?- find_entity_by_type_and_attribute('Restaurant', foodType, 'Fish', RestID).

% S4: Find POIs in a given city that match a category
find_pois_in_city_by_category(CityName, CategoryName, PoiID) :-
    poi_in_city(PoiID, CityName),         % Rule 11 from previous set
    poi_for_category(PoiID, CategoryName). % Rule 16 from previous set

% S5: Find POIs that are free and belong to a specific category
find_free_pois_by_category(CategoryName, PoiID) :-
    poi_is_free(PoiID),                   % Rule 7 from previous set
    poi_for_category(PoiID, CategoryName).

% S6: Find hotels in a city with a minimum star rating offering a specific service type
find_hotels_in_city_by_stars_and_service(CityName, MinStars, ServiceTypeName, HotelID, ServiceEntityID) :-
    entity(HotelID, 'Hotel', Attributes),
    member(attr(stars, Stars), Attributes),
    Stars >= MinStars,
    poi_in_city(HotelID, CityName), % Hotels are POIs
    hotel_offers_service_type(HotelID, ServiceEntityID, ServiceTypeName). % Rule 9

% S7: Search for transportation options between two (conceptual) places/POIs
% This is a simplified search, assuming direct transport or via a shared station.
% For more complex routing, a graph search algorithm would be needed.
% This rule finds means of transport available at places associated with POI1 and POI2.
% It doesn't guarantee a direct route, just co-located transport.

% Find transport available at the place of a POI
transport_at_poi_place(PoiID, TransportID, TransportType) :-
    poi_at_place(PoiID, PlaceID), % Get the PlaceID of the POI
    % Find stations at this PlaceID
    entity(StationID, 'Station', StationAttrs),
    member(attr(place, PlaceID), StationAttrs), % Station is at the same PlaceID
    member(attr(kind, StationKind), StationAttrs), % e.g., Train, Airport, Bus
    % Find transport means associated with this kind of station or type
    % This link is often implicit or needs more schema.
    % For now, let's find any available transport of a type that *could* use such a station.
    available_road_transport(TransportID, TransportType), % Road transport is generally available
    (TransportType = 'Bus' ; TransportType = 'Taxi'). % Example: Buses/Taxis are generally at places
    % Or, more specific to station kind:
transport_at_poi_place(PoiID, TransportID, TransportType) :-
    poi_at_place(PoiID, PlaceID),
    entity(StationID, 'Station', [attr(place, PlaceID), attr(kind, 'Train')]),
    entity(TransportID, TransportType, _), is_subclass(TransportType, 'RailTransportation').
transport_at_poi_place(PoiID, TransportID, TransportType) :-
    poi_at_place(PoiID, PlaceID),
    entity(StationID, 'Station', [attr(place, PlaceID), attr(kind, 'Airport')]),
    entity(TransportID, TransportType, _), is_subclass(TransportType, 'AirTransportation').

% S8: Find POIs relevant to a list of categories (POI must be relevant to ALL listed categories)
find_pois_for_multiple_categories([], _PoiID) :- !, fail. % Should not happen with proper call
find_pois_for_multiple_categories(CategoryList, PoiID) :-
    is_poi(PoiID),
    forall(member(CategoryName, CategoryList), poi_for_category(PoiID, CategoryName)).

% S9: Find "Luxury Cultural Experiences" - e.g. 4+ star Hotels near Cultural Sites, or highly-rated cultural sites
find_luxury_cultural_experiences(ExperienceID) :-
    is_luxury_hotel(ExperienceID), % Rule 4
    poi_at_place(ExperienceID, HotelPlaceID),
    is_cultural_site(CulturalSiteID), % Rule 12
    poi_at_place(CulturalSiteID, CulturalSitePlaceID),
    HotelPlaceID == CulturalSitePlaceID. % Simplistic: same general place ID
    % Could add a distance check if lat/long were consistently used.
find_luxury_cultural_experiences(ExperienceID) :-
    is_cultural_site(ExperienceID),
    get_attribute_value(ExperienceID, estimatedCost, Cost),
    (Cost == default ; Cost >= 50). % Example: "luxury" cultural site if expensive or unknown cost

% S10: Content-based recommendation: Find POIs of the same specific type and in the same city as a given POI
recommend_similar_poi(GivenPoiID, RecommendedPoiID) :-
    is_poi(GivenPoiID),
    entity(GivenPoiID, Type, _),
    poi_in_city(GivenPoiID, CityName),
    is_poi(RecommendedPoiID),
    RecommendedPoiID \== GivenPoiID, % Not the same POI
    entity(RecommendedPoiID, Type, _), % Same specific type
    poi_in_city(RecommendedPoiID, CityName). % In the same city

% S11: Search for attractions by material and technique
find_attractions_by_material_technique(Material, Technique, AttractionID) :-
    entity(AttractionID, 'Attraction', Attributes),
    (Material = '*' ; member(attr(material, MatVal), Attributes), sub_string(MatVal, _,_,_, Material)),
    (Technique = '*' ; member(attr(technique, TechVal), Attributes), sub_string(TechVal, _,_,_, Technique)).
% Use '*' as a wildcard for material or technique.

% S12: Find visits planned by a specific person within a budget range
find_visits_by_planner_and_budget(PersonID, MinBudget, MaxBudget, VisitID) :-
    visit_planner(VisitID, PersonID), % Rule 17
    entity(VisitID, 'Visit', Attributes),
    member(attr(budget, Budget), Attributes),
    Budget >= MinBudget,
    Budget =< MaxBudget.


% --- QUERY EXAMPLES ---
/*
?- is_poi(louvre).
   true.
?- is_poi(non_existent_place).
   false.

?- is_eating_place(trattoria_romana).
   true.
?- is_eating_place(louvre).
   false.

?- restaurant_food_type(R, 'Generic').
   R = trattoria_romana ;
   R = kyoto_ramen.
?- restaurant_food_type(sushi_master_kyoto, FoodType).
   FoodType = 'Fish'.

?- is_luxury_hotel(H).ƒ
   H = grand_hotel_paris ;
   H = ryokan_kyoto.ƒ

?- is_budget_hotel(budget_inn_rome).
   true.

?- poi_requires_ticket(eiffel_tower).
   true.
?- poi_requires_ticket(city_park_paris).
   false.

?- poi_is_free(trevi_fountain).
   true.
?- poi_is_free(city_park_paris).
   true.

?- poi_cheaper_than(Poi, 19).
   Poi = colosseum ;
   Poi = kyoto_ramen.

?- hotel_offers_service_type(grand_hotel_paris, ServiceID, 'Yoga').
   ServiceID = ghp_yoga_class.
?- hotel_offers_service_type(Hotel, ServiceID, 'Spa').
   Hotel = grand_hotel_paris, ServiceID = ghp_spa_service ;
   Hotel = ryokan_kyoto,    ServiceID = ryokan_onsen.


?- poi_at_place(Poi, p1).
   Poi = louvre ;
   Poi = eiffel_tower ;
   Poi = city_park_paris ;
   Poi = le_bistro_parisien ;
   Poi = grand_hotel_paris ;
   Poi = pharmacy_central_paris ;
   Poi = bank_of_paris.


?- poi_in_city(louvre, City).
   City = 'Paris City Center'.
?- poi_in_city(Poi, 'Rome Historic Center').
   Poi = colosseum ;
   Poi = trevi_fountain ;
   Poi = trattoria_romana ;
   Poi = budget_inn_rome.


?- is_cultural_site(louvre).
   true.
?- is_cultural_site(colosseum).
   true.
?- is_cultural_site(trattoria_romana).
   false.

?- is_relaxation_spot(Spot).
   Spot = ghp_spa_service ;    % (from entity type Spa/Resort)
   Spot = ryokan_onsen ;       % (from entity type Spa)
   Spot = city_park_paris ;    % (from entity type Park)
   Spot = ghp_yoga_class ;     % (from entity type Yoga)
   Spot = ghp_spa_service ;    % (from hotel_offers_service_type Spa)
   Spot = ryokan_onsen ;       % (from hotel_offers_service_type Spa)
   Spot = ghp_yoga_class.      % (from hotel_offers_service_type Yoga)
   % Note: some results might appear multiple times if they satisfy multiple conditions. Use setof/3 for unique results.
   % Example: setof(Spot, is_relaxation_spot(Spot), UniqueSpots).

?- is_road_vehicle(my_sedan).
   true.
?- is_road_vehicle(paris_metro_line1).
   false.

?- entity(bike1, 'Bike', []). % Add a bike fact for testing
%  true.
?- is_eco_friendly_transport(bike1).
%  true.

?- poi_for_category(Poi, 'Art').
   Poi = louvre.
?- poi_for_category(colosseum, CategoryName).
   CategoryName = 'History' ;
   CategoryName = 'Architecture'.

?- visit_planner(paris_trip_alice, Person).
   Person = person1.

?- poi_in_person_visit(person1, Poi).
   Poi = louvre ;
   Poi = eiffel_tower.

?- attraction_material(mona_lisa, 'Oil').
   true.
?- attraction_material(mona_lisa, 'Wood').
   true.
?- attraction_material(david_statue, Material).
   Material = 'Marble'.

?- attraction_by_developer(Attraction, artist_davinci).
   Attraction = mona_lisa.

?- is_service_poi(pharmacy_central_paris).
   true.
?- is_service_poi(bank_of_paris).
   true.
?- is_service_poi(louvre).
   false.

?- is_place_to_visit_poi(louvre).
   true.
?- is_place_to_visit_poi(eiffel_tower).
   true.
?- is_place_to_visit_poi(pharmacy_central_paris).
   false.


?- hotel_offers_beauty_service(grand_hotel_paris, ServiceID, BeautyServiceType).
   ServiceID = ghp_spa_service, BeautyServiceType = 'Resort'. % Resort is a Spa, Spa is a BeautyService

% Add a Thai massage service fact for a hotel if not already covered by a generic Spa that *is* Thai.
?- entity(ghp_thai_massage, 'Thai', [attr(name, 'Authentic Thai Massage')]).
%  true.
?- rel(wasIn, ghp_thai_massage, grand_hotel_paris, [attr(reason, 'specialized_service')]).
%  true.
?- offers_thai_massage(grand_hotel_paris).
%  true.
% Let's add a standalone Thai massage place
?- entity(thai_spa_paris, 'Thai', [attr(name, 'Siam Wellness Paris'), attr(place, p1)]).
%  true.
?- offers_thai_massage(thai_spa_paris).
%  true.

?- poi_has_time_limit(eiffel_tower, Limit).
   Limit = 180.
?- poi_has_time_limit(louvre, _).
   false.

?- available_road_transport(ID, Type).
   ID = my_sedan, Type = 'Car' ;
   ID = city_bus_101, Type = 'Bus'.

% Add Pizzeria/Stakehouse facts
?- entity(pizza_place, 'Pizzeria', [attr(name, 'Luigi Pizzeria'), attr(place, p2)]).
%  true.
?- entity(steak_house, 'Stakehouse', [attr(name, 'The Big Steak'), attr(place, p1)]).
%  true.
?- is_pizzeria_or_stakehouse(pizza_place).
%  true.
?- is_pizzeria_or_stakehouse(steak_house).
%  true.
?- is_pizzeria_or_stakehouse(trattoria_romana).
%  false.

% Add religious site facts
?- entity(notre_dame, 'Cathedral', [attr(name, 'Notre Dame Cathedral'), attr(place, p1)]). % Cathedral is subclass of Church, Church is PlaceToVisit
%  true. % Notre Dame is a Cathedral, but to be a ReligiousService:
?- entity(st_pauls_service, 'Catholic', [attr(name, 'St Pauls Sunday Mass Location'), attr(place, p1)]).
%  true.
?- is_religious_site(st_pauls_service, Type).
%  Type = 'Catholic'.

% Add shop facts
?- entity(gift_shop_louvre, 'Shop', [attr(name, 'Louvre Gift Shop'), attr(place, p1), attr(type, 'Gifts')]).
%  true.
?- entity(fashion_store_paris, 'Shop', [attr(name, 'Chic Boutique'), attr(place, p1), attr(type, 'Clothes')]).
%  true.
?- shop_of_type(Shop, 'Gifts').
%  Shop = gift_shop_louvre.
?- shop_of_type(fashion_store_paris, Type).
%  Type = 'Clothes'.

?- station_serves_transport_kind(paris_main_station, Kind).
   Kind = 'Train'.
?- station_serves_transport_kind(S, 'Airport').
   S = rome_fco_airport.

?- get_attribute_value(louvre, phone, Phone).
   Phone = '+33 1 40 20 50 50'.
?- get_attribute_value(eiffel_tower, phone, Phone).
   Phone = default. % As no phone attribute was defined for eiffel_tower

?- get_attribute_value(eiffel_tower, name, Name).
   Name = 'Eiffel Tower'.
*/
/*

% --- Examples for Search Rules ---

?- find_poi_by_name('Louvre Museum', ID).
% Expected: ID = louvre.

?- find_poi_by_partial_name('tower', ID), get_entity_display_name(ID, Name).
% Expected: ID = eiffel_tower, Name = 'Eiffel Tower'. (and others if they exist)

?- find_entity_by_type_and_attribute('Hotel', stars, 5, HotelID), get_entity_display_name(HotelID, Name).
% Expected: HotelID = grand_hotel_paris, Name = 'Grand Hotel Paris'.

?- find_pois_in_city_by_category('Paris City Center', 'Art', PoiID), get_entity_display_name(PoiID, Name).
% Expected: PoiID = louvre, Name = 'Louvre Museum'.

?- find_free_pois_by_category('History', PoiID), get_entity_display_name(PoiID, Name).
% Expected: (depends on facts, e.g. if Trevi Fountain was categorized as History and is free)

?- find_hotels_in_city_by_stars_and_service('Paris City Center', 4, 'Spa', HotelID, ServiceID), get_entity_display_name(HotelID, HotelName), get_entity_display_name(ServiceID, ServiceName).
% Expected: HotelID = grand_hotel_paris, ServiceID = ghp_spa_service, HotelName = 'Grand Hotel Paris', ServiceName = 'Hotel Main Spa'. (Assuming ghp_spa_service is type Spa)

?- find_pois_for_multiple_categories(['Art', 'Architecture'], PoiID), get_entity_display_name(PoiID, Name).
% Expected: PoiID = louvre, Name = 'Louvre Museum'. (if Louvre is in both)

?- find_luxury_cultural_experiences(ID), get_entity_display_name(ID, Name).
% Expected: (e.g. grand_hotel_paris if it's near a cultural site, or expensive cultural sites)

?- recommend_similar_poi(louvre, RecID), get_entity_display_name(RecID, RecName).
% Expected: (any other 'Museum' in 'Paris City Center')

?- find_attractions_by_material_technique('Marble', '*', AttID), get_entity_display_name(AttID, Name).
% Expected: AttID = david_statue, Name = 'Statue of David'.

?- find_attractions_by_material_technique('*', 'Oil on poplar panel', AttID), get_entity_display_name(AttID, Name).
% Expected: AttID = mona_lisa, Name = 'Mona Lisa'.

?- find_visits_by_planner_and_budget(person1, 1500, 2500, VisitID), get_entity_display_name(VisitID, Name).
% Expected: VisitID = paris_trip_alice, Name = 'Alice Paris Trip'.

To find all Museums in Paris that are free:

poi_with_custom_filters(
       PoiID,
       'Paris City Center', 'Museum',  % City, EntityType
       _, _,                            % Category, Age (disabled)
       _, 17, _,                        % TimeLimit (disabled), MaxCost = 17, EntranceFee (disabled)
       true, true,                       % Enable City, Enable EntityType
       false, false,                     % Disable Category, Disable Age
       false, true, false                % Disable TimeLimit, Enable MaxCost, Disable EntranceFee
   ), get_entity_display_name(PoiID, Name).

*/
% --- END OF FILE ---