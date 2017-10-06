create table groups (
	group_id int,
	group_name varchar(50),
	primary key (group_id)
);

create table students (
	student_id int,
	first_name varchar(50),
	second_name varchar(50),
	group_id int,
	primary key (student_id),
	foreign key (group_id) references groups (group_id)
);

create table subjects (
	subject_id int,
	subject_name varchar(50),
	teacher_id int,
	primary key (subject_id)
);

create table grades (
	student_id int,
	subject_id int,
	grade int,
	primary key (student_id, subject_id),
	foreign key (student_id) references students (student_id),
	foreign key (subject_id) references subjects (subject_id)
);

create table teachers (
	teacher_id int,
	first_name varchar(50),
	second_name varchar(50),
	subject_id int,
	primary key (teacher_id)
);

create table subjectsTeachers (
	subject_id int,
	teacher_id int,
	primary key (subject_id, teacher_id),
	foreign key (subject_id) references subjects (subject_id),
	foreign key (teacher_id) references teachers (teacher_id)
);

alter table subjects
	add foreign key (subject_id, teacher_id) references subjectsTeachers (subject_id, teacher_id);

alter table teachers
	add foreign key (subject_id, teacher_id) references subjectsTeachers (subject_id, teacher_id);
