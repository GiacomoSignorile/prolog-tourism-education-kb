:- use_module(library(lists)).
:- discontiguous inverse_of/2.
:- discontiguous subclass_of/2.
:- discontiguous entity/3.
:- discontiguous rel/4.
:- discontiguous place_info/5.
:- discontiguous person_info/2.
:- discontiguous category_info/2.
:- discontiguous collection_info/2.
:- discontiguous event_info/3.
:- dynamic user_preference/2.
:- discontiguous can_travel_directly/3.
:- discontiguous is_reachable/2.
:- discontiguous is_reachable/3.

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

% -- Entity Attribute Schemas --
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
'PointOfInterest'([place, address, phone, estimatedCost]).
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
'PlaceToVisit'([timeLimit, requiresTicket]).
'Archive'([]).
'Architecture'([]).
'Church'([]).
'Library'([]).
'Monument'([]).
'Museum'([]).
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

% -- Relationship Inverse Definitions --
inverse_of('includes', 'belongsTo').
inverse_of('developedBy', 'developed').
inverse_of('kindOf', 'isA').
inverse_of('madeBy', 'makes').
inverse_of('ownedBy', 'owned').
inverse_of('pertains', 'relevantFor').
inverse_of('hosted', 'wasIn').

% -- Relationship Signature Schemas --
'belongsTo'(['Attraction'-'Collection', 'PointOfInterest'-'Category', 'PointOfInterest'-'Collection'], []).
'developed'(['Person'-'Attraction'], [role, order]).
'isA'(['Artifact'-'Attraction', 'Place'-'PointOfInterest'], []).
'makes'(['Collection'-'Visit', 'Collection'-'Event', 'Person'-'Visit'], [role]).
'owned'(['Organization'-'Attraction', 'Person'-'Attraction'], [quantity, startDate, endDate, public]).
'relevantFor'(['Attraction'-'Category', 'Category'-'Attraction', 'Collection'-'Category', 'PointOfInterest'-'Category', 'Person'-'PointOfInterest', 'Category'-'PointOfInterest'], [role]).
'wasIn'(['Attraction'-'Place', 'Attraction'-'PointOfInterest', 'Collection'-'PointOfInterest', 'Person'-'PointOfInterest', 'PointOfInterest'-'Place', 'Visit'-'Event', 'Visit'-'Place'], [reason, address, startDate, endDate]).


% --- GENERIC SCHEMA UTILITY RULES ---

is_subclass(Class, Class) :- !. 
is_subclass(Subclass, Superclass) :-
  is_subclass_normal(Subclass, Superclass).
is_subclass(Subclass, Superclass) :-
  \+ is_subclass_normal(Subclass, _), 
  current_predicate(inverse_of/2),    
  (inverse_of(Subclass, _) ; inverse_of(_, Subclass)), 
  is_subclass_inverse(Subclass, Superclass).

is_subclass_normal(Subclass, Superclass) :-
  subclass_of(Subclass, Superclass).
is_subclass_normal(Subclass, Superclass) :-
  subclass_of(Subclass, Middleclass),
  is_subclass_normal(Middleclass, Superclass).

is_subclass_inverse(SubclassInverse, SuperclassInverse) :-
  invert_relationship(SubclassInverse, SubclassClause), 
  SubclassClause =.. [SubclassName|_],                
  is_subclass_normal(SubclassName, SuperclassName),   
  invert_relationship(SuperclassName, SuperclassClauseInverse), 
  SuperclassClauseInverse =.. [SuperclassInverse|_].       

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
invert_relationship(RelationshipName, RelationshipClause) :- 
  \+ inverse_of(_, RelationshipName),
  \+ inverse_of(RelationshipName, _),
  schema_relationship_definition(RelationshipName, SubjObjList, AttributeList),
  RelationshipClause =.. [RelationshipName, SubjObjList, AttributeList].

schema_relationship_definition(RelName, SubjObjList, AttrList) :-
    Clause =.. [RelName, SubjObjList, AttrList],
    current_predicate(RelName/2), 
    call(Clause).

invert_subj_obj([], []).
invert_subj_obj([Subject-Object|T1], [Object-Subject|T2]) :-
  invert_subj_obj(T1, T2).

gather_attributes(ClassName, AllAttributes) :-
    ClassName =.. [NameAtom], 
    (   current_predicate(NameAtom/1) -> 
        SchemaClause =.. [NameAtom, DirectAttributes],
        call(SchemaClause),
        findall(SuperC, is_subclass(NameAtom, SuperC), ParentsListWithSelf),
        remove_self(NameAtom, ParentsListWithSelf, ParentsList),
        gather_parent_attributes_entity(ParentsList, ParentAttributesLists),
        flatten([ParentAttributesLists, DirectAttributes], FlatAttributes),
        list_to_set(FlatAttributes, AllAttributes) 
    ;   current_predicate(NameAtom/2) -> 
        SchemaClause =.. [NameAtom, _References, DirectAttributes],
        call(SchemaClause),
        findall(SuperC, is_subclass(NameAtom, SuperC), ParentsListWithSelf),
        remove_self(NameAtom, ParentsListWithSelf, ParentsList),
        gather_parent_attributes_relationship(ParentsList, ParentAttributesLists),
        flatten([ParentAttributesLists, DirectAttributes], FlatAttributes),
        list_to_set(FlatAttributes, AllAttributes)
    ;   inverse_of(NameAtom, BaseRel) -> 
        gather_attributes(BaseRel, AllAttributes) 
    ;   AllAttributes = [] 
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
gather_references(RelationshipName, AllReferences) :-
    RelationshipName =.. [NameAtom],
    (   current_predicate(NameAtom/2) -> 
        SchemaClause =.. [NameAtom, DirectReferences, _Attributes],
        call(SchemaClause),
        findall(SuperR, is_subclass(NameAtom, SuperR), ParentsListWithSelf),
        remove_self(NameAtom, ParentsListWithSelf, ParentsList),
        gather_parent_references_relationship(ParentsList, ParentReferencesLists),
        flatten([ParentReferencesLists, DirectReferences], FlatReferences),
        list_to_set(FlatReferences, AllReferences)
    ;   inverse_of(NameAtom, BaseRel) -> 
        gather_references(BaseRel, BaseReferences),
        invert_subj_obj(BaseReferences, AllReferences) 
    ;   AllReferences = [] 
    ).

gather_parent_references_relationship([], []).
gather_parent_references_relationship([Parent|RestParents], AllParentReferences) :-
    ParentSchema =.. [Parent, ParentDirectReferences, _ParentAttrs],
    (   call(ParentSchema) -> true ; ParentDirectReferences = [] ),
    gather_parent_references_relationship(RestParents, RestReferences),
    append(ParentDirectReferences, RestReferences, AllParentReferences).


% --- INSTANCE FACTS ---

place_info(p1, 'Paris City Center', '1 Rue de Rivoli, 75001 Paris, France', 48.8566, 2.3522).
place_info(p2, 'Rome Historic Center', 'Piazza Navona, 00186 Rome, Italy', 41.8992, 12.4731).
place_info(p3, 'Kyoto Gion District', 'Gion, Higashiyama Ward, Kyoto, Japan', 35.0030, 135.7780).

person_info(person1, 'Alice Wonderland').
person_info(person2, 'Bob The Builder').
person_info(artist_davinci, 'Leonardo da Vinci').

category_info(cat_art, 'Art').
category_info(cat_history, 'History').
category_info(cat_foodie, 'Foodie Experience').
category_info(cat_relax, 'Relaxation').
category_info(cat_architecture, 'Architecture').

collection_info(col1, 'Paris Must-See Art').
collection_info(col2, 'Roman Holiday Itinerary').

event_info(event_paris_fest, 'Paris Summer Festival', date(2024,7,14)).

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

entity(museum_modern_art, 'Museum', [attr(name, 'Museum of Modern Art'), attr(place, p1), attr(estimatedCost, 0), attr(requiresTicket, false)]).
entity(museum_nature_paris, 'Museum', [attr(name, 'Natural History Museum'), attr(place, p1), attr(estimatedCost, 0), attr(requiresTicket, false)]).


entity(museum_roman_history, 'Museum', [attr(name, 'Museum of Roman History'), attr(place, p2), attr(estimatedCost, 0), attr(requiresTicket, false)]).

entity(museum_kyoto_art, 'Museum', [attr(name, 'Kyoto Art Museum'), attr(place, p3), attr(estimatedCost, 0), attr(requiresTicket, false)]).
serves_poi(paris_metro_line1, louvre).
serves_poi(paris_metro_line1, musee_orsay).
entity(paris_metro_line1, 'RailTransportation', [attr(name, 'Paris Metro Line 1')]).

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



% --- DOMAIN-SPECIFIC RULES (30) ---

% Helper to get a single attribute value from an entity's attribute list
get_attribute_value(EntityID, AttrName, Value) :-
    entity(EntityID, _, Attributes),
    member(attr(AttrName, Value), Attributes).
get_attribute_value(EntityID, AttrName, default) :- 
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
    entity(RestaurantID, 'Restaurant', Attributes), 
    member(attr(foodType, FoodType), Attributes).

% General rule to find hotels based on star rating and an operator
hotel_with_star_rating(HotelID, Operator, Value) :-
    entity(HotelID, 'Hotel', Attributes),       
    member(attr(stars, ActualStars), Attributes), 
    number(ActualStars),                        
    number(Value),                             
    ComparisonGoal =.. [Operator, ActualStars, Value],
    call(ComparisonGoal).

% 4. Find luxury hotels (e.g., 4 stars or more)
is_luxury_hotel(HotelID) :-
    hotel_with_star_rating(HotelID, >=, 4).

% 5. Find budget hotels (e.g., 2 stars or less)
is_budget_hotel(HotelID) :-
    hotel_with_star_rating(HotelID, =<, 2). 

% 6. Check if a POI requires a ticket
poi_requires_ticket(PoiID) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, requiresTicket, true).

% 7. Check if a POI has free entry
poi_is_free(PoiID) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, requiresTicket, false).
poi_is_free(PoiID) :- 
    is_poi(PoiID),
    \+ get_attribute_value(PoiID, requiresTicket, true),
    get_attribute_value(PoiID, estimatedCost, Cost),
    Cost =:= 0.

% 8. Find POIs with an estimated cost below a certain budget
poi_cheaper_than(PoiID, MaxCost) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, estimatedCost, Cost),
    number(Cost),
    Cost < MaxCost.

% Defining types that, when found in a hotel, are hotel services 
is_typical_hotel_amenity_type(Type) :-
    member(Type, ['Spa', 'Resort', 'Yoga', 'Pilates', 'BikeTour', 'Restaurant', 'Bar']).

% 9. Find hotels offering a specific type of hotel service (e.g., 'Yoga', 'Spa')
hotel_offers_service_type(HotelID, ServiceEntityID, TargetServiceType) :-
    entity(HotelID, 'Hotel', _),
    rel(wasIn, ServiceEntityID, HotelID, _),
    entity(ServiceEntityID, ActualServiceEntityType, _),
    (   is_subclass(ActualServiceEntityType, 'HotelService') 
    ;   is_typical_hotel_amenity_type(ActualServiceEntityType)
    ),
    is_subclass(ActualServiceEntityType, TargetServiceType).

luxury_hotel_all_services(LuxuryHotelID, ListOfServiceIDs) :-
    is_luxury_hotel(LuxuryHotelID), 
    setof(ServiceID,
          _AnyServiceType^(hotel_offers_service_type(LuxuryHotelID, ServiceID, _AnyServiceType)),
          ListOfServiceIDs).

luxury_hotel_all_services_safe(LuxuryHotelID, ListOfServiceIDs) :-
    is_luxury_hotel(LuxuryHotelID),
    (   setof(ServiceID, _AnyServiceType^(hotel_offers_service_type(LuxuryHotelID, ServiceID, _AnyServiceType)), ListOfServiceIDs)
    ->  true
    ;   ListOfServiceIDs = [] 
    ).

% 10. Find POIs located at a specific place (using PlaceID from entity attribute)
poi_at_place(PoiID, PlaceID) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, place, PlaceID).

% 11. Find POIs in a specific city (by matching PlaceID to place_info name)
poi_in_city(PoiID, CityName) :-
    poi_at_place(PoiID, PlaceID), 
    place_info(PlaceID, CityName, _, _, _).

% 12. Generalize: is something a "cultural site"?
is_cultural_site(SiteID) :-
    entity(SiteID, Type, _),
    ( is_subclass(Type, 'Museum');
      is_subclass(Type, 'ArchaeologicalSite');
      is_subclass(Type, 'Monument');
      is_subclass(Type, 'Palace');
      is_subclass(Type, 'Theater');
      is_subclass(Type, 'Church'); 
      is_subclass(Type, 'Cathedral');
      is_subclass(Type, 'Library');
      is_subclass(Type, 'Archive')
    ).

% 13. Find relaxation spots (Spas, Parks, some types of HotelServices like Yoga) generalized with a list.
relaxation_type(Type) :- member(Type, ['Spa', 'Park', 'Yoga', 'Pilates', 'Resort', 'Massage']).
is_relaxation_spot(ID) :-
  entity(ID, T, _), relaxation_type(Base),
  is_subclass(T, Base).


% 14. Check if a vehicle is a road vehicle (e.g. Car, Bus, Taxi, Motorcycle, Scooter)
is_road_vehicle(VehicleID) :-
    entity(VehicleID, Type, _),
    is_subclass(Type, 'RoadTransportation').

% 15. Check if a transport is eco-friendly (e.g. Bike, Skateboard, Electric vehicles)
is_eco_friendly_transport(TransportID) :-
    entity(TransportID, Type, _),
    ( is_subclass(Type, 'Bike') ;
      is_subclass(Type, 'Skateboard');
      get_attribute_value(TransportID, typeOfFuel, 'Electric')
    ).

count_pois_for_category(CategoryName, Count) :-
    category_info(_CatID, CategoryName), 
    (   setof(PoiID, poi_for_category(PoiID, CategoryName), PoiList)
    ->  length(PoiList, Count)
    ;   Count = 0 
    ).

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
    rel(makes, PersonID, VisitID, _), 
    entity(VisitID, 'Visit', _),
    rel(makes, CollectionID, VisitID, _), 
    collection_info(CollectionID, _),
    rel(belongsTo, PoiID, CollectionID, _), 
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

% 21. Check if a POI is a type of 'Service' (e.g., Hospital, Pharmacy, Post Office)
is_service_poi(PoiID) :-
    entity(PoiID, Type, _),
    is_subclass(Type, 'Service').

% 22. Check if a POI is a place to visit (e.g., Museum, Park, Theater)
is_place_to_visit_poi(PoiID) :-
    entity(PoiID, Type, _),
    is_subclass(Type, 'PlaceToVisit').

% 23. Find hotels that offer a specific beauty service (e.g., 'Thai Massage')
hotel_offers_beauty_service(HotelID, ServiceEntityID, SpecificBeautyServiceType) :-
    hotel_offers_service_type(HotelID, ServiceEntityID, _AnyHotelServiceType), 
    entity(ServiceEntityID, SpecificBeautyServiceType, _), 
    is_subclass(SpecificBeautyServiceType, 'BeautyService'). 

% 24. Find places that offer Thai Massage
offers_thai_massage(PlaceID) :- 
    entity(PlaceID, 'Hotel', _), 
    hotel_offers_service_type(PlaceID, _ServiceEntityID, 'Thai').
offers_thai_massage(PlaceID) :- 
    entity(PlaceID, 'Thai', _), 
    is_subclass('Thai', 'BeautyService'). 

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

% 27. Check if a place is a pizzeria or steakhouse
is_pizzeria_or_stakehouse(PlaceID) :-
    entity(PlaceID, Type, _),
    (is_subclass(Type, 'Pizzeria') ; is_subclass(Type, 'Stakehouse')).

% 28. Find POIs that are religious sites (based on ReligiousService hierarchy)
is_religious_service_site(SiteID, SpecificReligionType) :-
    entity(SiteID, SpecificReligionType, _),
    is_subclass(SpecificReligionType, 'ReligiousService').

is_place_of_worship(SiteID, Type) :-
    entity(SiteID, Type, _),
    ( is_subclass(Type, 'Church');
      is_subclass(Type, 'Mosque');
      is_subclass(Type, 'Temple');
      is_subclass(Type, 'Shrine');
      is_subclass(Type, 'Synagogue') 
    ).


% 29. Find shops of a particular type (e.g., 'Gifts')
shop_of_type(ShopID, ShopType) :-
    entity(ShopID, 'Shop', Attributes), 
    member(attr(type, ShopType), Attributes).

% 30. Find stations serving a specific kind of transport (e.g., 'Train', 'Airport')
station_serves_transport_kind(StationID, Kind) :-
    entity(StationID, 'Station', Attributes), 
    member(attr(kind, Kind), Attributes).

% 31. Calculate the average cost of museums in a city
average_museum_cost_in_city(CityName, AverageCost) :-
    poi_in_city(MuseumID, CityName),         
    entity(MuseumID, 'Museum', _Attributes), 
    findall(Cost,
            (   poi_in_city(MID, CityName),
                entity(MID, 'Museum', _),
                get_attribute_value(MID, estimatedCost, Cost),
                number(Cost) 
            ),
            CostsList),
    CostsList \= [], 
    sum_list(CostsList, TotalCost),
    length(CostsList, NumberOfMuseums),
    AverageCost is TotalCost / NumberOfMuseums.

% For unique museums to avoid issues if poi_in_city yields duplicates 
average_museum_cost_in_city_unique(CityName, AverageCost) :-
    setof(Cost, MID^CityPoi^( 
                poi_in_city(MID, CityName),
                entity(MID, 'Museum', _),
                get_attribute_value(MID, estimatedCost, Cost),
                number(Cost)
            ), CostsList), 
    CostsList \= [],
    sum_list(CostsList, TotalCost),
    length(CostsList, NumberOfMuseums),
    AverageCost is TotalCost / NumberOfMuseums.

activity_for_age(_Category, Age, PoiID) :-
    entity(PoiID, _, Attributes),
    member(attr(minAge, Min), Attributes),
    Age >= Min.

% Custom filters for POIs based on user preferences (for example to search free museums in Paris, check usage query)
poi_with_custom_filters(
    PoiID,
    CityFilterValue, EntityTypeFilterValue,
    CategoryFilterValue, AgeFilterValue,   
    TimeLimitFilterValue, MaxCostFilterValue, EntranceFeeFilterValue,
    EN_City, EN_EntityType,
    EN_Category, EN_Age, 
    EN_TimeLimit, EN_MaxCost, EN_EntranceFee 
) :-
    is_poi(PoiID),
    (EN_City -> poi_in_city(PoiID, CityFilterValue) ; true),
    (EN_EntityType -> entity(PoiID, ActualType, _), is_subclass(ActualType, EntityTypeFilterValue) ; true),
    (EN_Category -> poi_for_category(PoiID, CategoryFilterValue) ; true),
    (EN_Age -> activity_for_age(_, AgeFilterValue, PoiID) ; true),
    (EN_TimeLimit -> poi_has_time_limit(PoiID, ActualTimeLimit), ActualTimeLimit =< TimeLimitFilterValue ; true),
    (EN_MaxCost -> poi_cheaper_than(PoiID, MaxCostFilterValue) ; true),
    (EN_EntranceFee -> filter_entrance_fee(PoiID, EntranceFeeFilterValue) ; true).

filter_entrance_fee(PoiID, free) :-
    poi_is_free(PoiID).
filter_entrance_fee(PoiID, paid) :-
    poi_requires_ticket(PoiID).
filter_entrance_fee(PoiID, MaxCost) :-
    number(MaxCost),
    get_attribute_value(PoiID, estimatedCost, Cost),
    (Cost == default -> true ; number(Cost), Cost =< MaxCost).
filter_entrance_fee(_, _). 



% --- DYNAMIC PREDICATES FOR USER PREFERENCES ---

add_preference(UserID, PoiID) :-
    person_info(UserID, _), 
    is_poi(PoiID),          
    (   user_preference(UserID, PoiID) 
    ->  format('INFO: ~w already liked ~w.~n', [UserID, PoiID]) 
    ;   assertz(user_preference(UserID, PoiID)), 
        format('Preference: ~w liked ~w added.~n', [UserID, PoiID])
    ), !. 

add_preference(UserID, _PoiID) :- 
    \+ person_info(UserID, _), 
    format('ERROR: User ~w not found. Preference not added.~n', [UserID]),
    !, fail. 

add_preference(UserID, PoiID) :- 
    person_info(UserID, _), 
    \+ is_poi(PoiID),       
    format('ERROR: POI ~w not found. Preference not added.~n', [PoiID]),
    !, fail. 


remove_preference(UserID, PoiID) :-
    user_preference(UserID, PoiID), 
    retract(user_preference(UserID, PoiID)),
    format('Preference: ~w unliked ~w removed.~n', [UserID, PoiID]).
remove_preference(UserID, PoiID) :-
    \+ user_preference(UserID, PoiID), 
    format('INFO: ~w did not have ~w as a preference. Nothing removed.~n', [UserID, PoiID]).

list_preferences(UserID) :-
    person_info(UserID, UserName),
    format('Preferences for ~w (~w):~n', [UserName, UserID]),
    findall(CurrentPoiID, user_preference(UserID, CurrentPoiID), AllPreferences),
    (   AllPreferences = []
    ->  format('  No preferences found for this user.~n', [])
    ;   forall(member(PoiToList, AllPreferences),
               (get_entity_display_name(PoiToList, PoiDisplayName),
                format('  - ~w (~w)~n', [PoiDisplayName, PoiToList]))
            )
    ).
list_preferences(UserID) :- 
    \+ person_info(UserID, _),
    format('ERROR: User ~w not found.~n', [UserID]).

% Rule to recommend a POI based on user preferences:
recommend_based_on_prefs(User, Recommendation) :-
    user_preference(User, LikedPOI),         
    entity(LikedPOI, _Type, _Attributes),    
    recommend_similar_poi(LikedPOI, Recommendation), 
    Recommendation \== LikedPOI,             
    \+ user_preference(User, Recommendation). 

% To get all recommendations for a user:
% ?- setof(R, recommend_based_on_prefs(person1, R), Recommendations).

% --- SEARCH RULES ---

get_entity_display_name(EntityID, DisplayName) :-
    get_attribute_value(EntityID, name, NameValue),
    NameValue \== default, 
    !,
    DisplayName = NameValue.
get_entity_display_name(EntityID, EntityID). 

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

% S4: Find POIs in a given city that match a category
find_pois_in_city_by_category(CityName, CategoryName, PoiID) :-
    poi_in_city(PoiID, CityName),         
    poi_for_category(PoiID, CategoryName). 

% S5: Find POIs that are free and belong to a specific category
find_free_pois_by_category(CategoryName, PoiID) :-
    poi_is_free(PoiID),                  t
    poi_for_category(PoiID, CategoryName).

% S6: Find hotels in a city with a minimum star rating offering a specific service type
find_hotels_in_city_by_stars_and_service(CityName, MinStars, ServiceTypeName, HotelID, ServiceEntityID) :-
    entity(HotelID, 'Hotel', Attributes),
    member(attr(stars, Stars), Attributes),
    Stars >= MinStars,
    poi_in_city(HotelID, CityName), 
    hotel_offers_service_type(HotelID, ServiceEntityID, ServiceTypeName). 

% S7: Search for transportation options between two (conceptual) places/POIs

% Find transport available at the place of a POI
transport_at_poi_place(PoiID, TransportID, TransportType) :-
    poi_at_place(PoiID, PlaceID), 
    entity(StationID, 'Station', StationAttrs),
    member(attr(place, PlaceID), StationAttrs), 
    member(attr(kind, StationKind), StationAttrs), 
    available_road_transport(TransportID, TransportType), 
    (TransportType = 'Bus' ; TransportType = 'Taxi'). 

transport_at_poi_place(PoiID, TransportID, TransportType) :-
    poi_at_place(PoiID, PlaceID),
    entity(StationID, 'Station', [attr(place, PlaceID), attr(kind, 'Train')]),
    entity(TransportID, TransportType, _), is_subclass(TransportType, 'RailTransportation').
transport_at_poi_place(PoiID, TransportID, TransportType) :-
    poi_at_place(PoiID, PlaceID),
    entity(StationID, 'Station', [attr(place, PlaceID), attr(kind, 'Airport')]),
    entity(TransportID, TransportType, _), is_subclass(TransportType, 'AirTransportation').

% S8: Find POIs relevant to a list of categories (POI must be relevant to ALL listed categories)
find_pois_for_multiple_categories([], _PoiID) :- !, fail. 
find_pois_for_multiple_categories(CategoryList, PoiID) :-
    is_poi(PoiID),
    forall(member(CategoryName, CategoryList), poi_for_category(PoiID, CategoryName)).

% S10: Content-based recommendation: Find POIs of the same specific type and in the same city as a given POI
recommend_similar_poi(GivenPoiID, RecommendedPoiID) :-
    is_poi(GivenPoiID),
    entity(GivenPoiID, Type, _),
    poi_in_city(GivenPoiID, CityName),
    is_poi(RecommendedPoiID),
    RecommendedPoiID \== GivenPoiID, 
    entity(RecommendedPoiID, Type, _), 
    poi_in_city(RecommendedPoiID, CityName). 

% S11: Search for attractions by material and technique
find_attractions_by_material_technique(Material, Technique, AttractionID) :-
    entity(AttractionID, 'Attraction', Attributes),
    (Material = '*' ; member(attr(material, MatVal), Attributes), sub_string(MatVal, _,_,_, Material)),
    (Technique = '*' ; member(attr(technique, TechVal), Attributes), sub_string(TechVal, _,_,_, Technique)).

% S12: Find visits planned by a specific person within a budget range
find_visits_by_planner_and_budget(PersonID, MinBudget, MaxBudget, VisitID) :-
    visit_planner(VisitID, PersonID), % Rule 17
    entity(VisitID, 'Visit', Attributes),
    member(attr(budget, Budget), Attributes),
    Budget >= MinBudget,
    Budget =< MaxBudget.

% --- DEFINING MOVES BETWEEN ENTITIES FOR DFS ---
% mov(Entity1, Entity2) is true if we can "move" from Entity1 to Entity2.

mov(Entity1, Entity2) :-
    rel(_RelName, Entity1, Entity2, _Attrs).

mov(Entity1, Entity2) :-
    (   inverse_of(InvRelName, RelName) -> % If RelName has a defined inverse
        rel(InvRelName, Entity2, Entity1, _Attrs)
    ; 
        rel(_RelName, Entity2, Entity1, _Attrs)
    ).

mov(AttractionID, PoiID) :-
    entity(AttractionID, TypeA, _), is_subclass(TypeA, 'Attraction'),
    entity(PoiID, TypeP, _), is_subclass(TypeP, 'PointOfInterest'),
    rel(wasIn, AttractionID, PoiID, _).
mov(PoiID, AttractionID) :- 
    entity(AttractionID, TypeA, _), is_subclass(TypeA, 'Attraction'),
    entity(PoiID, TypeP, _), is_subclass(TypeP, 'PointOfInterest'),
    rel(wasIn, AttractionID, PoiID, _).

mov(PoiID, PlaceID) :-
    entity(PoiID, TypeP, _), is_subclass(TypeP, 'PointOfInterest'),
    place_info(PlaceID, _, _, _, _), % Ensure PlaceID is a valid place
    rel(wasIn, PoiID, PlaceID, _).
mov(PlaceID, PoiID) :- % And its inverse
    entity(PoiID, TypeP, _), is_subclass(TypeP, 'PointOfInterest'),
    place_info(PlaceID, _, _, _, _),
    rel(wasIn, PoiID, PlaceID, _).

mov(PersonID, AttractionID) :- rel(developed, PersonID, AttractionID, _).
mov(AttractionID, PersonID) :- rel(developed, PersonID, AttractionID, _).

mov(EntityID, CollectionID) :- rel(belongsTo, EntityID, CollectionID, _).
mov(CollectionID, EntityID) :- rel(belongsTo, EntityID, CollectionID, _).

% --- LUGER & STUBBLEFIELD STYLE DFS ADAPTED FOR ENTITIES ---

% Stack operations (using simple lists)
empty_stack([]).

stack(Element, CurrentStack, [Element | CurrentStack]). 

member_stack(Element, [Element | _Rest]). 
member_stack(Element, [_Head | Rest]) :-
    member_stack(Element, Rest).

go(Start, Goal) :-
    empty_stack(Empty_been_list),
    stack(Start, Empty_been_list, Been_list),
    path(Start, Goal, Been_list).


path(Goal, Goal, Been_list) :-
    write('Path found:'), nl,
    reverse_print_stack_with_names(Been_list),
    !. 

path(State, Goal, Been_list) :-
    mov(State, Next),                     
    not(member_stack(Next, Been_list)),    
    stack(Next, Been_list, New_been_list), 
    path(Next, Goal, New_been_list).      


reverse_print_stack_with_names(S) :-
    empty_stack(S).
reverse_print_stack_with_names(S) :-
    stack(E, Rest, S), 
    reverse_print_stack_with_names(Rest), 
    get_entity_display_name_safe(E, DisplayName), 
    write(DisplayName), nl.


get_entity_display_name_safe(ID, Name) :- entity(ID, _, _), get_attribute_value(ID, name, NameValue), NameValue \== default, !, Name = NameValue.
get_entity_display_name_safe(ID, ID).

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

% --- QUERY EXAMPLES FOR DYNAMIC PREDICATES ---
/*
?- add_preference(person1, louvre).
   Preference: person1 liked louvre added.
   true.

?- add_preference(person1, eiffel_tower).
   Preference: person1 liked eiffel_tower added.
   true.

?- add_preference(person1, louvre). % Try adding again
   INFO: person1 already liked louvre.
   true.

?- add_preference(non_existent_user, louvre).
   ERROR: User non_existent_user not found. Preference not added.
   false.

?- add_preference(person1, non_existent_poi).
   ERROR: POI non_existent_poi not found. Preference not added.
   false.

?- list_preferences(person1).
   Preferences for Alice Wonderland (person1):
     - Louvre Museum (louvre)
     - Eiffel Tower (eiffel_tower)
   true.f

?- user_preference(person1, X).
   X = louvre ;
   X = eiffel_tower.

% Assuming recommend_similar_poi(louvre, musee_orsay) is true
% and person1 has not liked musee_orsay yet.
?- recommend_based_on_prefs(person1, Recommendation).
   Recommendation = musee_orsay ; % If Louvre leads to Musee d'Orsay
   ... % Other recommendations based on Eiffel Tower, etc.

% To get unique recommendations:
?- setof(R, recommend_based_on_prefs(person1, R), Recommendations).
   Recommendations = [musee_orsay]. % Example output

?- remove_preference(person1, eiffel_tower).
   Preference: person1 unliked eiffel_tower removed.
   true.

?- user_preference(person1, eiffel_tower).
   false.

?- list_preferences(person1).
   Preferences for Alice Wonderland (person1):
     - Louvre Museum (louvre)
   true.

?- remove_preference(person1, colosseum). % Trying to remove something not preferred
   INFO: person1 did not have colosseum as a preference. Nothing removed.
   true.

   ?- is_reachable(louvre, eiffel_tower).
% Expected: true (if both are in Paris City Center)

    ?- is_reachable(louvre, colosseum).
    % Expected: false (with the current simple can_travel_directly/3)
    % To make this true, you'd need to define inter-city travel in can_travel_directly/3
    % e.g., louvre -> paris_main_station, paris_main_station -> rome_station (by train), rome_station -> colosseum

    % Example: Find all POIs reachable from the Louvre
    ?- findall(Dest, is_reachable(louvre, Dest), ReachableFromLouvre).
*/
% --- END OF FILE ---