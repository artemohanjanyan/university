drop schema public cascade; 
create schema public;

-----------
-- Types --
-----------

create type GameVariant as enum
(   'classical'
,   'chess960'
,   'crazyhouse'
,   'antichess'
);

create type TimeControlMethod as enum
(   'fischer'
,   'bronstein'
,   'daily'
,   'unlimited'
);

create type TimeControlParameters as
(   base_time       int
,   incr_per_move   int
);

create type TimeControl as
(   method          TimeControlMethod
,   parameters      TimeControlParameters   
);

create type Side as enum
(   'white'
,   'black'
);

create type GameResult as enum
(   'white'
,   'black'
,   'draw'
);

create type RatingType as enum
(   'bullet'
,   'blitz'
,   'rapid'
,   'classical'
,   'correspondence'
,   'chess960'
,   'crazyhouse'
,   'antichess'
,   'puzzle'
);

------------
-- Tables --
------------

create table Users
(   user_id         serial          primary key
,   user_name       varchar(50)     not null
,   email           varchar(254)    not null
,   password        bigint          not null
,   user_country    varchar(2)
,   registered      timestamp       not null    default current_timestamp
); 
create index on Users (user_name);
create index on Users (user_country);

create table Game
(   game_id         serial          primary key
,   game_variant    GameVariant     not null
,   rating_type     RatingType
,   time_control    TimeControl     not null
,   game_result     GameResult      not null
,   start_time      timestamp       not null
,   duration        interval        not null
,   game_record     json            not null
,   white_id        int             not null    references Users
,   black_id        int             not null    references Users
,   check (white_id != black_id)
);
create index on Game (white_id);
create index on Game (black_id);
create index on Game (game_variant);

create table Tournament
(   tournament_id   serial          primary key
,   tournament_name varchar(50)     not null
,   game_variant    GameVariant     not null
,   rating_type     RatingType
,   time_control    TimeControl     not null
,   start_time      timestamp       not null
,   duration        interval        not null
,   admin_id        int             not null    references Users
);
create index on Tournament (admin_id);
create index on Tournament (tournament_name);
create index on Tournament (game_variant);

create table TournamentParticipation
(   tournament_id   int             not null    references Tournament
,   user_id         int             not null    references Users
,   primary key (tournament_id, user_id)
);

create table TournamentGame
(   tournament_id   int             not null    references Tournament
,   game_id         int             primary key references Game
);

create table SimultaneousExhibition
(   simul_id        serial          primary key
,   game_variant    GameVariant     not null
,   host_side       side
,   time_control    TimeControl     not null
,   handicap        TimeControlParameters
,   start_time      timestamp       not null
,   host_id         int             not null    references Users
);
create index on SimultaneousExhibition (host_id);

create table SimulParticipation
(   simul_id        int             references SimultaneousExhibition
,   user_id         int             references Users
,   primary key (simul_id, user_id)
);

create table SimulGame
(   simul_id        int             not null    references SimultaneousExhibition
,   game_id         int             primary key references Game
);

create table Club
(   club_id         serial          primary key
,   club_name       varchar(50)     not null
,   club_country    varchar(2)
,   admin_id        int             not null    references Users
);
create index on Club (admin_id);

create table ClubMember
(   club_id         int             references Club
,   user_id         int             references Users
,   primary key (club_id, user_id)
);

create table Follows
(   who             int             references Users
,   whom            int             references Users
,   primary key (who, whom)
);

create table Rating
(   user_id         int             references Users
,   rating_type     RatingType
,   rating          decimal(6, 2)   not null
,   time_stamp      timestamp
,   primary key (user_id, rating_type, time_stamp)
);

create table Puzzle
(   puzzle_id       serial          primary key
,   puzzle_record   json            not null
,   source_id       int             references Game
,   complexity      decimal(6, 2)   not null
);
create index on Puzzle (source_id);

create table PuzzleAttempt
(   puzzle_id       int             references Puzzle
,   user_id         int             references Users
,   solved          boolean         not null
,   attempt_time    timestamp       not null
,   primary key (puzzle_id, user_id)
);

-----------
-- Views --
-----------

create view CurrentRating as
    select a.user_id, a.rating_type, a.rating, a.time_stamp
    from
        Rating a join
        (   select user_id, rating_type, max(time_stamp) as time_stamp
            from Rating
            group by (user_id, rating_type)
        ) b
        on a.user_id = b.user_id and a.rating_type = b.rating_type and a.time_stamp = b.time_stamp;

create view MaxRating as
    select a.user_id, a.rating_type, a.rating, a.time_stamp
    from
        Rating a join
        (   select user_id, rating_type, max(rating) as rating
            from Rating
            group by (user_id, rating_type)
        ) b
        on a.user_id = b.user_id and a.rating_type = b.rating_type and a.rating = b.rating;

create view Friends as
    select a.who as user1, a.whom as user2
    from follows a, follows b
    where a.who = b.whom and a.whom = b.who;

create view CountryLeaders as
    select 
        u.user_id, u.user_name, u.email, u.password, u.user_country, u.registered,
        u.rating_type, u.rating, u.time_stamp
    from
        (Users natural join CurrentRating) u join
        (   select user_country, rating_type, max(rating) as max_rating
            from Users natural join CurrentRating
            group by user_country, rating_type
        ) b
        on u.user_country = b.user_country and u.rating_type = b.rating_type and u.rating = b.max_rating;

create view GamesWithRatings as
    select *
    from
        (
            (
                select game_id,
                       game_variant,
                       g.rating_type,
                       time_control,
                       game_result,
                       start_time,
                       duration,
                       game_record,
                       white_id,
                       black_id,
                       start_time + duration as time_stamp,
                       max(wb.time_stamp) as white_rating_timestamp_before
                from Game g, Rating wb
                where g.white_id = wb.user_id and
                      g.rating_type = wb.rating_type and
                      wb.time_stamp < start_time + duration
                group by (g.game_id)
            ) as a
            natural join
            (
                select game_id, max(bb.time_stamp) as black_rating_timestamp_before
                from Game g, Rating bb
                where g.black_id = bb.user_id and
                      g.rating_type = bb.rating_type and
                      bb.time_stamp < start_time + duration
                group by (g.game_id)
            ) as b
        ) as g
        natural join
        (
            select user_id as white_id,
                   rating_type,
                   rating as white_rating_before,
                   time_stamp as white_rating_timestamp_before
            from Rating
        ) as a
        natural join
        (
            select user_id as black_id,
                   rating_type,
                   rating as black_rating_before,
                   time_stamp as black_rating_timestamp_before
            from Rating
        ) as b
        natural join
        (
            select user_id as white_id,
                   rating_type,
                   rating as white_rating_after,
                   time_stamp
            from Rating
        ) as c
        natural join
        (
            select user_id as black_id,
                   rating_type,
                   rating as black_rating_after,
                   time_stamp
            from Rating
        ) as d
        ;

---------------
-- Functions --
---------------

create or replace function CurrentRating(id int, rating_t RatingType) returns decimal(6, 2)
as $$
declare
    r decimal(6, 2);
begin
    select rating into r from CurrentRating where user_id = id and rating_type = rating_t;
    return coalesce(r, 1500);
end;
$$ language plpgsql;

create or replace function TournamentPerfomance(u_id int, t_id int) returns table(game_id int, score decimal(2, 1))
as $$
    select
        tg.game_id,
        case
            when game_result = 'white' then cast(cast(white_id = u_id as int) as decimal(2, 1))
            when game_result = 'draw' then cast (0.5 as decimal(2, 1))
            else cast(cast(black_id = u_id as int) as decimal(2, 1))
        end
    from (tournamentgame natural join game) tg join tournament t on t.tournament_id = tg.tournament_id
    where t_id = t.tournament_id and (white_id = u_id or black_id = u_id);
$$ language sql;

create or replace function Elo(wBefore decimal(6, 2), bBefore decimal(6, 2), result GameResult,
                               out wAfter decimal(6, 2), out bAfter decimal(6, 2))
as $$
declare
    r1  double precision;
    r2  double precision;
    e1  double precision;
    e2  double precision;
    s1  double precision;
    s2  double precision;
begin
    r1 = 10 ^ (cast(wBefore as double precision) / 400);
    r2 = 10 ^ (cast(bBefore as double precision) / 400);
    e1 = r1 / (r1 + r2);
    e2 = r2 / (r1 + r2);
    if result = 'white' then
        s1 = 1;
    elseif result = 'draw' then
        s1 = 0.5;
    else
        s1 = 0;
    end if;
    s2 = 1 - s1;
    wAfter = wBefore + cast(32 * (s1 - e1) as decimal(6, 2));
    bAfter = bBefore + cast(32 * (s2 - e2) as decimal(6, 2));
end;
$$ language plpgsql;

create or replace function DoAddGameTrigger() returns trigger
as $$
declare
    newRatingWhite decimal(6, 2);
    newRatingBlack decimal(6, 2);
begin
    if new.rating_type is null then
        return new;
    end if;
    select * into newRatingWhite, newRatingBlack
    from Elo(CurrentRating(new.white_id, new.rating_type),
             CurrentRating(new.black_id, new.rating_type),
             new.game_result);
    insert into Rating (user_id, rating_type, rating, time_stamp) values
    (new.white_id, new.rating_type, newRatingWhite, new.start_time + new.duration),
    (new.black_id, new.rating_type, newRatingBlack, new.start_time + new.duration);
    return new;
end;
$$ language plpgsql;
create trigger AddGameTrigger
after insert on Game
for each row execute procedure DoAddGameTrigger();

create or replace function DoAddPuzzleAttemptTrigger() returns trigger
as $$
declare
    newRating decimal(6, 2);
    puzzle_rating decimal(6, 2);
    q text;
begin transaction isolation level read uncommited;
    select * from current_setting('transaction_access') into q;
    raise notice '%', q;

    select complexity into puzzle_rating
    from Puzzle natural join PuzzleAttempt
    where puzzle_id = new.puzzle_id;
    select * into newRating, puzzle_rating
    from Elo(CurrentRating(new.user_id, 'puzzle'),
             puzzle_rating,
             cast(case when new.solved then 'white' else 'black' end as GameResult));
    insert into Rating (user_id, rating_type, rating, time_stamp) values
    (new.user_id, 'puzzle', newRating, new.attempt_time);
    return new;
end;
$$ language plpgsql;
create trigger AddPuzzleAttemptTrigger
after insert on PuzzleAttempt
for each row execute procedure DoAddPuzzleAttemptTrigger();

create or replace function DoCheckIsPuzzleAttempted() returns trigger
as $$
declare
    newRating decimal(6, 2);
    puzzle_rating decimal(6, 2);
    tmp int;
begin
    select user_id into tmp
    from PuzzleAttempt
    where puzzle_id = new.puzzle_id and user_id = new.user_id;
    if tmp is null then
        return new;
    else
        return null;
    end if;
end;
$$ language plpgsql;
create trigger CheckIsPuzzleAttempted
before insert on PuzzleAttempt
for each row execute procedure DoCheckIsPuzzleAttempted();

create or replace function DoCheckGameForTournament() returns trigger
as $$
declare
    w_id int;
    b_id int;
begin
    select white_id, black_id into w_id, b_id
    from Game g, Tournament t
    where new.tournament_id = t.tournament_id and
          new.game_id = g.game_id and
          g.game_variant = t.game_variant and
          g.rating_type = t.rating_type and
          g.time_control = t.time_control;
    if w_id is null then
        raise exception 'game config does not match';
    end if;
    if not exists (select * from TournamentParticipation
            where user_id = w_id and tournament_id = new.tournament_id) then
        raise exception 'white side does not participate in the tournament';
    end if;
    if not exists (select * from TournamentParticipation
            where user_id = b_id and tournament_id = new.tournament_id) then
        raise exception 'black side does not participate in the tournament';
    end if;
    return new;
end;
$$ language plpgsql;
create trigger CheckGameForTournament
before insert on TournamentGame
for each row execute procedure DoCheckGameForTournament();

create or replace function DoCheckGameForSimul() returns trigger
as $$
declare
    w_id int;
    b_id int;
begin
    select white_id, black_id into w_id, b_id
    from Game g, SimultaneousExhibition t
    where new.simul_id = t.simul_id and
          new.game_id = g.game_id and
          g.game_variant = t.game_variant and
          g.time_control = t.time_control;
    if w_id != new.host_id and b_id != new.host_id then
        raise exception 'host should participate in the game';
    end if;
    if not new.host_side is null and
            ((w_id == new.host_id and new.host_side == 'black') or
             (b_id == new.host_id and new.host_side == 'white')) then
        raise exception 'host_side is different in the game';
    end if;
    return new;
end;
$$ language plpgsql;
create trigger CheckGameForSimul
before insert on SimulGame
for each row execute procedure DoCheckGameForSimul();

create or replace function DoCheckSimulParticipation() returns trigger
as $$
declare
    h_id int;
begin
    select host_id into h_id
    from SimultaneousExhibition t
    where new.simul_id = t.simul_id;
    if h_id == new.user_id then
        raise exception 'host cannot participate';
    end if;
    return new;
end;
$$ language plpgsql;
create trigger CheckSimulParticipation
before insert on SimulParticipation
for each row execute procedure DoCheckSimulParticipation();
