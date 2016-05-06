package ru.ifmo.android_2015.db;

/**
 * Определения реляционной модели данных городов.
 */
public final class CityContract {

    /**
     * Колонки таблицы гордов.
     *
     * Из базового интерфейса BaseColumn используется колонка _ID -- ID строки SQLite таблицы,
     * в котором хранится ID города.
     */
    public interface CityColumns {

        /**
         * ID города, основной ключ
         *
         * SQLite type: INTEGER PRIMARY KEY
         */
        String CITY_ID = "_id";

        /**
         * Название города.
         *
         * SQLite type: TEXT
         */
        String NAME = "name";

        /**
         * Двухбуквенный код страны.
         *
         * SQLite type: TEXT
         */
        String COUNTRY = "country";

        /**
         * Широта в градусах.
         *
         * SQLite type: REAL
         */
        String LATITUDE = "latitude";

        /**
         * Долгота в градусах.
         *
         * SQLite type: REAL
         */
        String LONGITUDE = "longitude";
    }

    /**
     * Определение таблицы городов.
     *
     * Примечеание: этот класс определен как "implements CityColumns" для удобства, чтобы
     *              константы интерфейса оказались в области видимости этого класса.
     */
    public static final class Cities implements CityColumns {

        /**
         * Название таблицы городов.
         */
        public static final String TABLE = "cities";

        static final String CREATE_TABLE = "CREATE TABLE " + TABLE
                + " ("
                + CITY_ID + " INTEGER PRIMARY KEY, "
                + NAME + " TEXT, "
                + COUNTRY + " TEXT, "
                + LATITUDE + " REAL, "
                + LONGITUDE + " REAL"
                + " )";

    }

    private CityContract() {}
}
