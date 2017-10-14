insert into groups
	(group_id, group_name) values
	(1, 'M3439'),
	(2, 'M3438');

insert into students
	(student_id, student_name, group_id) values
	(1, 'Арина Александрова', 1),
	(2, 'Александр Востоков', 1),
	(3, 'Ведана Воронина',    2);

insert into courses
	(course_id, course_name) values
	(1, 'Базы данных'                    ),
	(2, 'Функциональное программирование');

insert into grades
	(student_id, course_id, grade) values
	(1, 1, 100),
	(1, 2, 99.99),
	(2, 1, 99.98),
	(3, 2, 99.96);

insert into lecturers
	(lecturer_id, lecturer_name) values
	(1, 'Георгий Корнеев'),
	(2, 'Дмитрий Коваников');

insert into teaching
	(group_id, course_id, lecturer_id) values
	(1, 1, 1),
	(1, 2, 2),
	(2, 1, 1),
	(2, 2, 2);
