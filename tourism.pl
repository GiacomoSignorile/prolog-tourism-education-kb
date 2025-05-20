:- use_module(library(lists)).
:- discontiguous inverse_of/2.
:- discontiguous subclass_of/2.
:- discontiguous entity/3.
:- discontiguous rel/4.
:- discontiguous place_info/5.
:- discontiguous person_info/4.
:- discontiguous category_info/2.
:- discontiguous collection_info/2.
:- discontiguous event_info/3.

% --- START OF SCHEMA (Copied from your input) ---
% ENTITIES
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
subclass_of('Attraction', 'Entity').
subclass_of('MeansOfTransportation', 'Entity').
subclass_of('PointOfInterest', 'Entity').
subclass_of('Visit', 'Entity').

% Entities schema (simplified for brevity, not strictly needed for rules if we use entity/3 facts)
% ... (Original entity schema definitions like 'Attraction'([date, technique, material]) are good for validation but not directly used by the rules below)

% RELATIONSHIPS
inverse_of('includes', 'belongsTo').
inverse_of('developedBy', 'developed').
inverse_of('kindOf', 'isA').
inverse_of('madeBy', 'makes').
inverse_of('ownedBy', 'owned').
inverse_of('pertains', 'relevantFor').
inverse_of('hosted', 'wasIn').

% GENERIC RULES (Copied from your input)
is_subclass(Subclass, Superclass) :-
  is_subclass_normal(Subclass, Superclass).
is_subclass(Subclass, Superclass) :-
  \+ is_subclass_normal(Subclass, _),
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

% (Keeping invert_relationship and gather_attributes/references as they were,
%  though they are more for schema introspection than direct domain reasoning)
invert_relationship(RelationshipName, InvertedRelationshipClause) :-
  schema_relationship(RelationshipName, SubjObjList, AttributeList), % Use a helper to get schema
  inverse_of(InvertedRelationshipName, RelationshipName),
  !,
  invert_subj_obj(SubjObjList, InvertedSubjObjList),
  InvertedRelationshipClause =.. [InvertedRelationshipName, InvertedSubjObjList, AttributeList].
invert_relationship(InvertedRelationshipName, RelationshipClause) :-
  schema_relationship(RelationshipName, SubjObjList, AttributeList), % Use a helper
  inverse_of(InvertedRelationshipName, RelationshipName),
  !,
  RelationshipClause =.. [RelationshipName, SubjObjList, AttributeList].
invert_relationship(RelationshipName, Relationship) :-
  \+ inverse_of(_, RelationshipName),
  \+ inverse_of(RelationshipName, _),
  schema_relationship(RelationshipName, SubjObjList, AttributeList), % Use a helper
  Relationship =.. [RelationshipName, SubjObjList, AttributeList].

% Helper to get relationship schema (needed for invert_relationship to work with the provided structure)
% This matches the structure from the original schema file like: 'belongsTo'(['Attraction'-'Collection', ...], []).
schema_relationship(RelName, SubjObjList, AttrList) :-
    Clause =.. [RelName, SubjObjList, AttrList],
    current_predicate(RelName/2), % Check if the schema fact exists
    call(Clause).

'belongsTo'(['Attraction'-'Collection', 'PointOfInterest'-'Category', 'PointOfInterest'-'Collection'], []).
'developed'(['Person'-'Attraction'], [ruolo, order]).
'isA'(['Artifact'-'Attraction', 'Place'-'PointOfInterest'], []).
'makes'(['Collection'-'Visit', 'Collection'-'Event', 'Person'-'Visit'], [role]).
'owned'(['Organization'-'Attraction', 'Person'-'Attraction'], [quantity, startDate, endDate, public]).
'relevantFor'(['Attraction'-'Category', 'Category'-'Attraction', 'Collection'-'Category', 'PointOfInterest'-'Category', 'Person'-'PointOfInterest', 'Category'-'PointOfInterest'], [role]).
'wasIn'(['Attraction'-'Place', 'Attraction'-'PointOfInterest', 'Collection'-'PointOfInterest', 'Person'-'PointOfInterest', 'PointOfInterest'-'Place', 'Visit'-'Event', 'Visit'-'Place'], [reason, address, startDate, endDate]).


invert_subj_obj([], []).
invert_subj_obj([Subject-Object|T1], [Object-Subject|T2]) :-
  invert_subj_obj(T1, T2).

% --- END OF SCHEMA ---


% --- FACTS ---

% Place Info (PlaceID, Name, AddressString, Lat, Long)
place_info(p1, 'Paris City Center', '1 Rue de Rivoli, 75001 Paris, France', 48.8566, 2.3522).
place_info(p2, 'Rome Historic Center', 'Piazza Navona, 00186 Rome, Italy', 41.8992, 12.4731).
place_info(p3, 'Kyoto Gion District', 'Gion, Higashiyama Ward, Kyoto, Japan', 35.0030, 135.7780).

% Person Info (PersonID, Name)
person_info(person1, 'Alice Wonderland').
person_info(person2, 'Bob The Builder').
person_info(artist_davinci, 'Leonardo da Vinci').

% Category Info (CategoryID, Name)
category_info(cat_art, 'Art').
category_info(cat_history, 'History').
category_info(cat_foodie, 'Foodie Experience').
category_info(cat_relax, 'Relaxation').
category_info(cat_architecture, 'Architecture').

% Collection Info (CollectionID, Name)
collection_info(col1, 'Paris Must-See Art').
collection_info(col2, 'Roman Holiday Itinerary').

% Event Info (EventID, Name, Date)
event_info(event_paris_fest, 'Paris Summer Festival', date(2024,7,14)).

% Entities (EntityID, EntityType, ListOfAttributes)
entity(louvre, 'Museum', [attr(name, 'Louvre Museum'), attr(place, p1), attr(estimatedCost, 20), attr(requiresTicket, true), attr(phone, '+33 1 40 20 50 50')]).
entity(eiffel_tower, 'Tower', [attr(name, 'Eiffel Tower'), attr(place, p1), attr(estimatedCost, 25), attr(requiresTicket, true), attr(timeLimit, 180)]).
entity(colosseum, 'ArchaeologicalSite', [attr(name, 'Colosseum'), attr(place, p2), attr(estimatedCost, 18), attr(requiresTicket, true)]).
entity(trevi_fountain, 'Fountain', [attr(name, 'Trevi Fountain'), attr(place, p2), attr(estimatedCost, 0), attr(requiresTicket, false)]).
entity(city_park_paris, 'Park', [attr(name, 'Jardin du Luxembourg'), attr(place, p1), attr(estimatedCost, 0), attr(requiresTicket, false)]).

entity(trattoria_romana, 'Restaurant', [attr(name, 'Trattoria Romana'), attr(place, p2), attr(foodType, 'Generic'), attr(type, 'Italian'), attr(estimatedCost, 40)]).
entity(le_bistro_parisien, 'Bistro', [attr(name, 'Le Bistro Parisien'), attr(place, p1), attr(estimatedCost, 60), attr(requiresTicket, false)]).
entity(kyoto_ramen, 'Restaurant', [attr(name, 'Kyoto Ramen Shop'), attr(place, p3), attr(foodType, 'Generic'), attr(type, 'Japanese'), attr(estimatedCost, 15)]).
entity(sushi_master_kyoto, 'Restaurant', [attr(name, 'Sushi Master Kyoto'), attr(place, p3), attr(foodType, 'Fish'), attr(type, 'Japanese High-End'), attr(estimatedCost, 150)]).


entity(grand_hotel_paris, 'Hotel', [attr(name, 'Grand Hotel Paris'), attr(place, p1), attr(stars, 5), attr(phone, '+33 1 23 45 67 89')]).
entity(budget_inn_rome, 'Hotel', [attr(name, 'Budget Inn Rome'), attr(place, p2), attr(stars, 2)]).
entity(ryokan_kyoto, 'Hotel', [attr(name, 'Gion Ryokan'), attr(place, p3), attr(stars, 4)]).

entity(ghp_yoga_class, 'Yoga', [attr(name, 'Sunrise Yoga')]).
entity(ghp_spa_service, 'Resort', [attr(name, 'Hotel Main Spa')]). % Resort is a subtype of Spa
entity(ghp_cooking_class, 'CookingClass', [attr(name, 'French Pastry Class')]).
entity(ghp_bike_tour, 'BikeTour', [attr(name, 'Paris City Bike Tour')]).
entity(ryokan_onsen, 'Spa', [attr(name, 'Private Onsen Experience')]). % Spa, not a specific subtype here
entity(ryokan_tea_ceremony, 'HotelService', [attr(name, 'Traditional Tea Ceremony')]). % Generic hotel service

entity(paris_main_station, 'Station', [attr(name, 'Gare du Nord'), attr(place, p1), attr(kind, 'Train')]).
entity(rome_fco_airport, 'Station', [attr(name, 'Fiumicino Airport'), attr(place, p2), attr(kind, 'Airport')]).

entity(my_sedan, 'Car', [attr(type, 'Sedan'), attr(availability, true), attr(consumption, '7L/100km'), attr(typeOfFuel, 'Gasoline')]).
entity(city_bus_101, 'Bus', [attr(availability, true), attr(consumption, '20L/100km'), attr(typeOfFuel, 'Diesel')]).
entity(paris_metro_line1, 'RailTransportation', [attr(transportType, 'Subway'), attr(availability, true)]).

entity(mona_lisa, 'Attraction', [attr(name, 'Mona Lisa'), attr(date, '1503-1506'), attr(technique, 'Oil on poplar panel'), attr(material, 'Oil paint, Poplar wood')]).
entity(david_statue, 'Attraction', [attr(name, 'Statue of David'), attr(date, '1501-1504'), attr(technique, 'Sculpture'), attr(material, 'Marble')]).

entity(paris_trip_alice, 'Visit', [attr(startDate, date(2024,8,1)), attr(endDate, date(2024,8,7)), attr(budget, 2000)]).
entity(rome_weekend_bob, 'Visit', [attr(startDate, date(2024,9,10)), attr(endDate, date(2024,9,12)), attr(budget, 500)]).

entity(pharmacy_central_paris, 'Pharmacy', [attr(name, 'Central Pharmacy Paris'), attr(place, p1), attr(phone, '+33 1 98 76 54 32')]).
entity(bank_of_paris, 'Bank', [attr(name, 'Bank of Paris'), attr(place, p1)]).

% Relationships (RelationshipName, SubjectID, ObjectID, ListOfAttributes)
rel(wasIn, mona_lisa, louvre, []). % Mona Lisa is in Louvre
rel(wasIn, louvre, p1, []). % Louvre is at place p1
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

rel(belongsTo, louvre, col1, []). % Louvre belongs to "Paris Must-See Art" collection
rel(belongsTo, eiffel_tower, col1, []).
rel(belongsTo, colosseum, col2, []).
rel(belongsTo, trevi_fountain, col2, []).

rel(makes, col1, paris_trip_alice, [attr(role, 'itinerary_basis')]). % Collection "Paris Must-See Art" makes up Alice's Paris trip
rel(makes, person1, paris_trip_alice, [attr(role, 'planner')]). % Alice planned her Paris trip
rel(makes, person2, rome_weekend_bob, [attr(role, 'traveler')]).

rel(developed, artist_davinci, mona_lisa, [attr(role, 'painter')]). % Da Vinci developed Mona Lisa

rel(isA, p1, louvre, []). % The place p1 is (represented by) the Louvre. (Interpretation of isA Place-PointOfInterest)
                         % Or, more accurately, the POI refers to a Place entity.
                         % The schema says 'Place'-'PointOfInterest'. This means a specific named Place (like 'Louvre Building') *is a* PointOfInterest.
                         % My entity(louvre, 'Museum', [attr(place, p1)...]) already links the POI to a generic place_info(p1, ...).
                         % Let's use 'isA' for Artifact-Attraction for now.
                         % If an artifact 'mona_lisa_artifact' is the attraction 'mona_lisa_attraction_entry'
% For 'isA' Place-PointOfInterest: this means a specific place (e.g. 'Eiffel Tower Structure') is a PointOfInterest.
% This is slightly different from a POI *being at* a place.
% Let's assume we have specific Place entities that are also POIs.
% entity(eiffel_tower_struct, 'Place', [attr(name, 'Eiffel Tower Structure')]).
% rel(isA, eiffel_tower_struct, eiffel_tower, []). % The Eiffel Tower Structure (a Place) is the POI Eiffel Tower.

% For 'isA' Artifact-Attraction:
% entity(mona_lisa_artifact_obj, 'Artifact', [attr(description, 'The physical painting')]).
% rel(isA, mona_lisa_artifact_obj, mona_lisa, []). % The artifact object is the attraction Mona Lisa.


% --- RULES (30) ---

% Helper to get a single attribute value
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
    entity(RestaurantID, 'Restaurant', Attributes),
    member(attr(foodType, FoodType), Attributes).

% 4. Find luxury hotels (e.g., 4 stars or more)
is_luxury_hotel(HotelID) :-
    entity(HotelID, 'Hotel', Attributes),
    member(attr(stars, Stars), Attributes),
    Stars >= 4.

% 5. Find budget hotels (e.g., 2 stars or less)
is_budget_hotel(HotelID) :-
    entity(HotelID, 'Hotel', Attributes),
    member(attr(stars, Stars), Attributes),
    Stars =< 2.

% 6. Check if a POI requires a ticket
poi_requires_ticket(PoiID) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, requiresTicket, true).

% 7. Check if a POI has free entry
poi_is_free(PoiID) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, requiresTicket, false).
poi_is_free(PoiID) :- % Also consider free if cost is 0
    is_poi(PoiID),
    get_attribute_value(PoiID, estimatedCost, Cost),
    Cost =:= 0.


% 8. Find POIs with an estimated cost below a certain budget
poi_cheaper_than(PoiID, MaxCost) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, estimatedCost, Cost),
    number(Cost), % Ensure Cost is a number
    Cost < MaxCost.

% 9. Find hotels offering a specific type of hotel service (e.g., 'Yoga', 'Spa')
hotel_offers_service_type(HotelID, ServiceEntityID, ServiceType) :-
    entity(HotelID, 'Hotel', _),
    rel(wasIn, ServiceEntityID, HotelID, _), % A service entity is 'in' the hotel
    entity(ServiceEntityID, ActualServiceType, _),
    is_subclass(ActualServiceType, 'HotelService'),
    is_subclass(ActualServiceType, ServiceType). % ServiceType can be 'Yoga', 'Spa', or even 'HotelService' itself

% 10. Find POIs located at a specific place (using PlaceID)
poi_at_place(PoiID, PlaceID) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, place, PlaceID).

% 11. Find POIs in a specific city (by matching PlaceID to place_info name)
poi_in_city(PoiID, CityName) :-
    poi_at_place(PoiID, PlaceID),
    place_info(PlaceID, CityName, _, _, _).

% 12. Generalize: is something a "cultural site"? (Museum, ArchSite, Monument, Palace, Theater)
is_cultural_site(SiteID) :-
    entity(SiteID, Type, _),
    (is_subclass(Type, 'Museum');
     is_subclass(Type, 'ArchaeologicalSite');
     is_subclass(Type, 'Monument');
     is_subclass(Type, 'Palace');
     is_subclass(Type, 'Theater');
     is_subclass(Type, 'Church');
     is_subclass(Type, 'Cathedral')).

% 13. Find relaxation spots (Spas, Parks, some types of HotelServices like Yoga)
is_relaxation_spot(SpotID) :- entity(SpotID, Type, _), is_subclass(Type, 'Spa').
is_relaxation_spot(SpotID) :- entity(SpotID, Type, _), is_subclass(Type, 'Park').
is_relaxation_spot(SpotID) :- entity(SpotID, Type, _), (is_subclass(Type, 'Yoga'); is_subclass(Type, 'Pilates')).
is_relaxation_spot(SpotID) :- hotel_offers_service_type(_, SpotID, 'Spa'). % A spa service offered by a hotel
is_relaxation_spot(SpotID) :- hotel_offers_service_type(_, SpotID, 'Yoga'). % A yoga service

% 14. Is a transportation means a road vehicle?
is_road_vehicle(VehicleID) :-
    entity(VehicleID, Type, _),
    is_subclass(Type, 'RoadTransportation').

% 15. Is a transportation means eco-friendly (e.g. Bike, Skateboard, or electric)?
% (Schema doesn't have 'electric', so we'll stick to Bike/Skateboard for now)
is_eco_friendly_transport(TransportID) :-
    entity(TransportID, Type, _),
    (Type = 'Bike' ; Type = 'Skateboard' ; is_subclass(Type, 'Bike') ; is_subclass(Type, 'Skateboard')).
% Could be extended if typeOfFuel='Electric' was an option:
% is_eco_friendly_transport(TransportID) :-
% get_attribute_value(TransportID, typeOfFuel, 'Electric').

% 16. Find POIs relevant for a specific category (e.g., 'Art')
poi_for_category(PoiID, CategoryName) :-
    is_poi(PoiID),
    category_info(CatID, CategoryName),
    rel(relevantFor, PoiID, CatID, _).

% 17. Find a person who planned a specific visit
visit_planner(VisitID, PersonID) :-
    entity(VisitID, 'Visit', _),
    person_info(PersonID, _),
    rel(makes, PersonID, VisitID, Attributes),
    member(attr(role, 'planner'), Attributes).

% 18. Find all POIs included in a person's visit (via collections)
poi_in_person_visit(PersonID, PoiID) :-
    visit_planner(VisitID, PersonID), % Find a visit planned by the person
    rel(makes, CollectionID, VisitID, _), % Find collections that make up this visit
    collection_info(CollectionID, _),
    rel(belongsTo, PoiID, CollectionID, _), % Find POIs belonging to that collection
    is_poi(PoiID).

% 19. Is an attraction made of a specific material?
attraction_material(AttractionID, Material) :-
    entity(AttractionID, 'Attraction', Attributes),
    member(attr(material, ActualMaterial), Attributes),
    % Allow partial match for material string
    sub_string(ActualMaterial, _, _, _, Material).

% 20. Find attractions developed by a specific person (e.g. artist)
attraction_by_developer(AttractionID, PersonID) :-
    entity(AttractionID, 'Attraction', _),
    person_info(PersonID, _),
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
hotel_offers_beauty_service(HotelID, ServiceEntityID, BeautyServiceType) :-
    hotel_offers_service_type(HotelID, ServiceEntityID, ServiceType), % Rule 9
    entity(ServiceEntityID, BeautyServiceType, _),
    is_subclass(BeautyServiceType, 'BeautyService').

% 24. Find places that offer Thai Massage
offers_thai_massage(PlaceID) :- % PlaceID could be a hotel or a standalone spa
    entity(PlaceID, 'Hotel', _), % If it's a hotel
    hotel_offers_service_type(PlaceID, ServiceEntityID, 'Thai').
offers_thai_massage(PlaceID) :- % If it's a standalone BeautyService entity
    entity(PlaceID, 'Thai', _),
    is_subclass('Thai', 'BeautyService'). % Ensure it's a beauty service not something else also named Thai

% 25. Find POIs that have a time limit for visits
poi_has_time_limit(PoiID, TimeLimitMinutes) :-
    is_poi(PoiID),
    get_attribute_value(PoiID, timeLimit, TimeLimitMinutes),
    number(TimeLimitMinutes).

% 26. Find available road transportation
available_road_transport(TransportID, Type) :-
    entity(TransportID, Type, Attributes),
    is_subclass(Type, 'RoadTransportation'),
    member(attr(availability, true), Attributes).

% 27. Is an eating place a Pizzeria or a Stakehouse?
is_pizzeria_or_stakehouse(PlaceID) :-
    entity(PlaceID, Type, _),
    (Type = 'Pizzeria' ; Type = 'Stakehouse'). % Direct check as they are specific

% 28. Find POIs that are religious sites
is_religious_site(SiteID, ReligionType) :-
    entity(SiteID, ReligionType, _),
    is_subclass(ReligionType, 'ReligiousService').

% 29. Find shops of a particular type (e.g., 'Gifts')
shop_of_type(ShopID, ShopType) :-
    entity(ShopID, 'Shop', Attributes),
    member(attr(type, ShopType), Attributes).

% 30. Find stations serving a specific kind of transport (e.g., 'Train', 'Airport')
station_serves_transport_kind(StationID, Kind) :-
    entity(StationID, 'Station', Attributes),
    member(attr(kind, Kind), Attributes).

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

?- is_luxury_hotel(H).
   H = grand_hotel_paris ;
   H = ryokan_kyoto.

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