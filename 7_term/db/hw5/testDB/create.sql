drop schema public cascade; 
create schema public;

create table Groups (
        groupId int primary key, 
        groupName name not null); 

create table Students (
        studentId int primary key, 
        studentName name not null, 
        groupId int,
        foreign key (groupId) references Groups (groupId),
        unique (studentId, groupId));

create table Courses (
        courseId int primary key, 
        courseName name not null);

create table Lecturers (
        lecturerId int primary key, 
        lecturerName name not null);

create table Plan (
        groupId int, 
        courseId int, 
        lecturerId int,
        foreign key (groupId) references Groups (groupId),
        foreign key (courseId) references Courses (courseId),
        foreign key (lecturerId) references Lecturers (lecturerId),
        unique (groupId, courseId, lecturerId));

create table Marks (
        studentId int, 
        courseId int, 
        mark decimal(4, 1) check ((0 <= mark) and (mark <= 100)) not null,
        primary key (studentId, courseId),
        foreign key (studentId) references Students (studentId),
        foreign key (courseId) references Courses (courseId));
