create table groups (
	group_id int,
	group_name varchar(5),
	primary key (group_id)
);

create table students (
	student_id int,
	student_name varchar(50),
	group_id int,
	primary key (student_id),
	foreign key (group_id) references groups (group_id)
);

create table courses (
	course_id int,
	course_name varchar(50),
	primary key (course_id)
);

create table grades (
	student_id int,
	course_id int,
	grade decimal(5, 2),
	primary key (student_id, course_id),
	foreign key (student_id) references students (student_id),
	foreign key (course_id) references courses (course_id)
);

create table lecturers (
	lecturer_id int,
	lecturer_name varchar(50),
	primary key (lecturer_id)
);

create table teaching (
	group_id int,
	course_id int,
	lecturer_id int,
	primary key (group_id, course_id),
	foreign key (group_id) references groups (group_id),
	foreign key (course_id) references courses (course_id),
	foreign key (lecturer_id) references lecturers (lecturer_id)
);
