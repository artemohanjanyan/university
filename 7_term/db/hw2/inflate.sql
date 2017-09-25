insert into groups
	(group_id, group_name) values
	(1, 'M3439'),
	(2, 'M3438');

insert into students
	(student_id, first_name, second_name, group_id) values
	(1, 'Арина',     'Александрова', 1),
	(2, 'Александр', 'Востоков',     1),
	(3, 'Ведана',    'Воронина',     2);

insert into subjects
	(subject_id, subject_name, teacher_id) values
	(1, 'Базы данных',                     null),
	(2, 'Функциональное программирование', null);

insert into grades
	(student_id, subject_id, grade) values
	(1, 1, 99),
	(1, 2, 98),
	(2, 1, 97),
	(3, 2, 96);

insert into teachers
	(teacher_id, first_name, second_name, subject_id) values
	(1, 'Георгий', 'Корнеев',   null),
	(2, 'Дмитрий', 'Коваников', null),
	(3, 'Арсений', 'Серока',    null);

insert into subjectsTeachers
	(subject_id, teacher_id) values
	(1, 1),
	(2, 2),
	(2, 3);

update subjects
	set teacher_id = 1 where subject_id = 1;
update subjects
	set teacher_id = 2 where subject_id = 2;

update teachers
	set subject_id = 1 where teacher_id = 1;
update teachers
	set subject_id = 2 where teacher_id = 2;
update teachers
	set subject_id = 2 where teacher_id = 3;
