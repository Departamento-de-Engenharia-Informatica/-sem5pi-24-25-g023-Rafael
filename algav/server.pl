% Bibliotecas HTTP
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).

% Bibliotecas JSON
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

backend_url('https://localhost:5001/api/').

http:location(api, '/server', []).

% Criação de servidor HTTP em 'Port' que trata pedidos em JSON
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

%-------------------------OperationTypes - SurgerysIds----------------------------%
:- http_handler(api(surgeryids), get_surgery_ids, []).

% GET SurgeryIds - OperationTypes
get_surgery_ids(_Request):-
    get_operation_types_backend(OperationTypes),
    maplist(convert_operation_type_to_surgery_id, OperationTypes, SurgeryIds),
    maplist(convert_surgery_id_to_json, SurgeryIds, JSON_SurgeryIds),
    reply_json(json([surgery_ids=JSON_SurgeryIds]), [json_object(dict)]).

% GET OperationTypes Backend
get_operation_types_backend(OperationTypes) :-
    backend_url(URL),
    atom_concat(URL, 'operationtypes', URL_OperationTypes),
    setup_call_cleanup(
        http_open(URL_OperationTypes, In, [cert_verify_hook(cert_accept_any)]),
        (read_string(In, _, Response),
         atom_json_dict(Response, OperationTypes, [])),
        close(In)).

% Convert OperationType dictionary to surgery_id
convert_operation_type_to_surgery_id(OperationTypeDict, surgery_id(Id, Name)) :-
    Id = OperationTypeDict.get(id),
    Name = OperationTypeDict.get(name).

% Convert surgery_id to JSON
convert_surgery_id_to_json(surgery_id(Id, Name), json([id = Id, name = Name])).

surgery_id(Id, Name) :- convert_operation_type_to_surgery_id(_, surgery_id(Id, Name)).

%-------------------------OperationRequest - AssigmentSurgery--------------------------%
:- http_handler(api(assignmentsurgeries), get_assignment_surgeries, []).

% GET AssignmentSurgeries - OperationRequests
get_assignment_surgeries(_Request) :-
    get_operation_requests_backend(OperationRequests),
    maplist(convert_operation_request_to_surgery_id, OperationRequests, AssignmentSurgeries),
    maplist(convert_assignment_surgery_to_json, AssignmentSurgeries, JSON_AssignmentSurgeries),
    reply_json(json([assignment_surgeries = JSON_AssignmentSurgeries]), [json_object(dict)]).

% GET OperationRequests Backend
get_operation_requests_backend(OperationRequests) :-
    backend_url(URL),
    atom_concat(URL, 'operationRequests/filter', URL_OperationRequests),
    setup_call_cleanup(
        http_open(URL_OperationRequests, In, [cert_verify_hook(cert_accept_any)]),
        (read_string(In, _, Response),
         atom_json_dict(Response, OperationRequests, [])),
        close(In)).

% Convert OperationRequest to assignment_surgery
convert_operation_request_to_surgery_id(OperationRequestDict, assignment_surgery(Id, DoctorId)) :-
    Id = OperationRequestDict.get(id),
    DoctorId = OperationRequestDict.get(doctorId).

% Convert assignment_surgery to JSON
convert_assignment_surgery_to_json(assignment_surgery(Id, DoctorId), json([id = Id, doctorId = DoctorId])).

assignment_surgery(Id, DoctorId) :- convert_operation_request_to_surgery_id(_, assignment_surgery(Id, DoctorId)).

%-------------------------OperationTypes - Surgeries--------------------------%
:- http_handler(api(surgeries), get_surgeries, []).

% GET OperationTypes - Surgeries
get_surgeries(_Request):-
    get_operation_types_backend(OperationTypes),
    maplist(convert_operation_type_to_surgery, OperationTypes, Surgeries),
    maplist(convert_surgery_to_json, Surgeries, JSON_Surgeries),
    reply_json(json([surgeries=JSON_Surgeries]), [json_object(dict)]).

% Convert OperationType to surgery_id
convert_operation_type_to_surgery(OperationTypeDict, surgery(Name, TAnesthesia,TSurgery,TCleaning)) :-
    Name = OperationTypeDict.get(name),
    TAnesthesia = OperationTypeDict.get(anesthesiaTime),
    TSurgery = OperationTypeDict.get(surgeryTime),
    TCleaning = OperationTypeDict.get(cleaningTime).

% Convert surgery to JSON
convert_surgery_to_json(surgery(Name, TAnesthesia, TSurgery, TCleaning), json([name=Name, tAnesthesia=TAnesthesia, tSurgery=TSurgery, tCleaning=TCleaning])).

surgery(Name, TAnesthesia,TSurgery,TCleaning) :- convert_operation_type_to_surgery(_, surgery(Name, TAnesthesia,TSurgery,TCleaning)).

%------------------------------- Staffs --------------------------------%
:- http_handler(api(staffs), get_staffs, []).

% GET Staffs
get_staffs(_Request) :-
    get_staffs_operation_types_backend(StaffsOperationTypes),
    maplist(convert_staff_to_json, StaffsOperationTypes, StaffJsons),
    reply_json(json([staffs = StaffJsons]), [json_object(dict)]).

% GET Staffs Operation Types Backend
get_staffs_operation_types_backend(StaffsOperationTypes) :-
    backend_url(URL),
    atom_concat(URL, 'staffs/operationtypes', URL_StaffsOperationTypes),
    setup_call_cleanup(
        http_open(URL_StaffsOperationTypes, In, [cert_verify_hook(cert_accept_any)]),
        (read_string(In, _, Response),
         atom_json_dict(Response, StaffsOperationTypes, [])),
        close(In)
    ).

% Convert staff to JSON
convert_staff_to_json(StaffDict, 
    json([staffID = StaffID, 
          role = Role, 
          specialization = Specialization, 
          operationTypesName = OperationTypesName])) :-
    StaffID = StaffDict.get(staffID),
    Role = StaffDict.get(role),
    Specialization = StaffDict.get(specialization),
    OperationTypesName = StaffDict.get(operationTypesName).

% Convert staff to JSON
convert_staff_to_json(staff(StaffId, StaffRole, SpecializationName, MatchingOperations), 
                      json([staffID = StaffId, role = StaffRole, specialization = SpecializationName, operationTypesName = MatchingOperations])).

staff(StaffId, StaffRole, SpecializationName, MatchingOperations) :-
    staff_syntax(_, staff(StaffId, StaffRole, SpecializationName, MatchingOperations)).

% ------------------------- TimeTable Staff ---------------------------------
:- http_handler(api(timetables), get_timetables, []).

get_timetables(_Request) :-
    get_staffs_backend(Staffs),
    findall(Timetable,(member(Staff, Staffs),convert_staff_to_timetable(Staff, Timetable)),
        StaffTimetables),
    flatten(StaffTimetables, AllTimeTables),
    maplist(convert_timetable_to_json, AllTimeTables, JsonTimeTables),
    reply_json(json([timetables = JsonTimeTables]), [json_object(dict)]).

% GET Staffs Backend
get_staffs_backend(Staffs) :-
    backend_url(URL),
    atom_concat(URL, 'staffs/filter', URL_Staffs),
    http_open(URL_Staffs, In, [cert_verify_hook(cert_accept_any)]),
    read_string(In, _, Response),
    atom_json_dict(Response, Staffs, []),
    close(In).

% Convert time string to minutes
time_string_to_minutes(Time, TimeInMinutes) :-
    split_string(Time, ":", "", [HourPart, MinutePart | _]),
    atom_number(HourPart, HourNumber),
    atom_number(MinutePart, MinuteNumber),
    TimeInMinutes is HourNumber * 60 + MinuteNumber.

% Convert Staff to Timetable
convert_staff_to_timetable(StaffDict, TimeTables) :-
    AvailabilitySlots = StaffDict.get(availabilitySlots),
    StaffId = StaffDict.get(id),
    
    (AvailabilitySlots \= []
        ->  findall(
            timetable(StaffId, FormattedDate, (StartMinutes, EndMinutes)),
            (member(AvailabilitySlot, AvailabilitySlots),

             StartSlotTime = AvailabilitySlot.get(startTime),
             EndSlotTime = AvailabilitySlot.get(endTime),
             
             split_string(StartSlotTime, "T", "", [DatePortion | StartTimeComponents]),
             split_string(EndSlotTime, "T", "", [_ | EndTimeComponents]),

             split_string(DatePortion, "-", "", [Year, Month, Day]),
             atom_number(Day, DayNumber),
             atom_number(Month, MonthNumber),
             atom_number(Year, YearNumber),

             % YYYYMMDD
             FormattedDate is YearNumber * 10000 + MonthNumber * 100 + DayNumber,

             atomic_list_concat(StartTimeComponents, " ", StartTime),
             atomic_list_concat(EndTimeComponents, " ", EndTime),

             time_string_to_minutes(StartTime, StartMinutes),
             time_string_to_minutes(EndTime, EndMinutes)
            ),
            TimeTables
        );
        TimeTables = []
    ).

% Convert timetable to JSON
convert_timetable_to_json(timetable(StaffId, Day, (StartTime, EndTime)),
                          json([staffId = StaffId, day = Day, startTime = StartTime, endTime = EndTime])).

timetable(StaffId, Day, (StartTime, EndTime)) :-
    convert_staff_to_timetable(_, timetable(StaffId, Day, (StartTime, EndTime))).

% ---------------------- Agenda Operation Rooms - Appointments ----------------------
:- http_handler(api(agendaoperationrooms), get_agenda_operation_rooms, []).

get_agenda_operation_rooms(_Request) :-
    get_appointments_backend(Appointments),
    findall(AgendaOperationRoom, 
        (member(Appointment, Appointments), 
         convert_appointment_to_agenda_operation_room(Appointment, AgendaOperationRoom)),
        AgendasOperationRooms),
    maplist(agenda_operation_rooms_to_json, AgendasOperationRooms, JsonAgendaOperationRooms),
    reply_json(json([agenda_operation_rooms = JsonAgendaOperationRooms]), [json_object(dict)]).

% GET Appointments Backend
get_appointments_backend(Appointments) :-
    backend_url(URL),
    atom_concat(URL, 'appointments', URL_Appointments),
    setup_call_cleanup(
        http_open(URL_Appointments, In, [cert_verify_hook(cert_accept_any)]),
        (read_string(In, _, Response),
         atom_json_dict(Response, Appointments, [])),
        close(In)).

% Convert Appointment dictionary to surgery_id
convert_appointment_to_agenda_operation_room(AppointmentDict, agenda_operation_room(Room, FormattedDate, Agenda)) :-
    Room = AppointmentDict.get(surgeryRoomDto).get(number),
    DateAndTime = AppointmentDict.get(dateAndTime),

    %2024-11-15T14:30:00 Example
    split_string(DateAndTime, "T", "", [DatePortion, TimePortion]),
    split_string(DatePortion, "-", "", [Year, Month, Day]),
    atom_number(Year, YearNumber),
    atom_number(Month, MonthNumber),
    atom_number(Day, DayNumber),
    
    FormattedDate is YearNumber * 10000 + MonthNumber * 100 + DayNumber,

    OperationRequest = AppointmentDict.get(operationRequestDto),
    OperationRequestType = OperationRequest.get(operationType),
    OperationTypeId = OperationRequestType.get(id),
    EstimatedDuration = OperationRequestType.get(estimatedDuration),

    split_string(TimePortion, ":", "", [HourStr, MinuteStr, _]),
    atom_number(HourStr, Hour),
    atom_number(MinuteStr, Minute),
    
    StartMinutes is Hour * 60 + Minute,
    EndMinutes is StartMinutes + EstimatedDuration,

    Agenda = [[StartMinutes, EndMinutes, OperationTypeId]].

% Convert Operation Rooms to JSON
agenda_operation_rooms_to_json(agenda_operation_room(Room, Day, Agenda), json([room = Room, day = Day, agenda = Agenda])).

agenda_operation_room(Room, Day, Agenda) :-
    convert_appointment_to_agenda_operation_room(_, agenda_operation_room(Room, Day, Agenda)).


% ----------------------- Agenda Staff - Appointments -------------------------
:- http_handler(api(agendastaff), get_agenda_staffs, []).

% GET AgendaStaffs
get_agenda_staffs(_Request) :-
    get_appointments_staffs_backend(Appointments),
    findall(AgendaStaff, (member(Appointment, Appointments), convert_appointment_to_agenda_staff(Appointment, AgendaStaff)),AgendasStaff),
    maplist(convert_agenda_staff_to_json, AgendasStaff, JsonAgendaStaffs),
    reply_json(json([agenda_staffs = JsonAgendaStaffs]), [json_object(dict)]).

% GET Appointments Staffs Backend
get_appointments_staffs_backend(Appointments) :-
    backend_url(URL),
    atom_concat(URL, 'staffs/appointments', URL_Appointments),
    setup_call_cleanup(
        http_open(URL_Appointments, In, [cert_verify_hook(cert_accept_any)]),
        (read_string(In, _, Response),
         atom_json_dict(Response, Appointments, [])),
        close(In)).

% Convert Appointment Staff to Agenda Staff
convert_appointment_to_agenda_staff(Appointment, agenda_staff(StaffID, Day, AppointmentsStaff)) :-
    Appointment.staffID = StaffID,
    Appointment.day = Day,
    findall(appointment_detail(AppointmentId, StartTime, EndTime), 
        (member(AppointmentDetail, Appointment.appointmentsStaff),
         AppointmentDetail.appointmentId = AppointmentId,
         AppointmentDetail.startTime = StartTime,
         AppointmentDetail.endTime = EndTime), 
        AppointmentsStaff).

% Convert AgendaStaff to JSON Format
convert_agenda_staff_to_json(agenda_staff(StaffID, Day, AppointmentsStaff), 
    json([staffID = StaffID, day = Day, appointments = AppointmentsStaffJson])) :-
    maplist(convert_appointment_detail_to_json, AppointmentsStaff, AppointmentsStaffJson).

% Convert AppointmentDetail to JSON Format
convert_appointment_detail_to_json(appointment_detail(AppointmentId, StartTime, EndTime),
    json([appointmentId = AppointmentId, startTime = StartTime, endTime = EndTime])).

agenda_staff(StaffId, Day, Appointments) :-
    convert_agenda_staff_to_json(_, agenda_staff(StaffId, Day, Appointments)).

:- server(4000).