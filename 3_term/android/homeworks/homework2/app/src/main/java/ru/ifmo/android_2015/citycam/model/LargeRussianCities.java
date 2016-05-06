package ru.ifmo.android_2015.citycam.model;

/**
 * Статический список крупных российских (+немного из СНГ) городов.
 */
public final class LargeRussianCities {

    private static final int COUNT = 90;

    private static City[] cities = new City[COUNT];

    public static int getCount() {
        return 90;
    }

    public static City getCity(int position) {
        if (cities[position] == null) {
            cities[position] = createCity(position);
        }
        return cities[position];
    }

    private static City createCity(int position) {
        switch (position) {
            case 0: return new City("Абакан", 53.720976, 91.44242300000001);
            case 1: return new City("Архангельск", 64.539304, 40.518735);
            case 2: return new City("Астана", 71.430564, 51.128422);
            case 3: return new City("Астрахань", 46.347869, 48.033574 );
            case 4: return new City("Барнаул", 53.356132, 83.74961999999999);
            case 5: return new City("Белгород", 50.597467, 36.588849);
            case 6: return new City("Бийск", 52.541444, 85.219686 );
            case 7: return new City("Бишкек", 42.871027, 74.59452 );
            case 8: return new City("Благовещенск", 50.290658, 127.527173 );
            case 9: return new City("Братск", 56.151382, 101.634152 );
            case 10: return new City("Брянск", 53.2434, 34.364198);
            case 11: return new City("Великий Новгород", 58.521475, 31.275475);
            case 12: return new City("Владивосток", 43.134019, 131.928379);
            case 13: return new City("Владикавказ", 43.024122, 44.690476);
            case 14: return new City("Владимир", 56.129042, 40.40703 );
            case 15: return new City("Волгоград", 48.707103, 44.516939 );
            case 16: return new City("Вологда", 59.220492, 39.891568);
            case 17: return new City("Воронеж", 51.661535, 39.200287);
            case 18: return new City("Грозный", 43.317992, 45.698197 );
            case 19: return new City("Донецк", 48.015877, 37.80285 );
            case 20: return new City("Екатеринбург", 56.838002, 60.597295);
            case 21: return new City("Иваново", 57.000348, 40.973921);
            case 22: return new City("Ижевск", 56.852775, 53.211463 );
            case 23: return new City("Иркутск", 52.286387, 104.28066 );
            case 24: return new City("Казань", 55.795793, 49.106585 );
            case 25: return new City("Калининград", 55.916229, 37.854467);
            case 26: return new City("Калуга", 54.507014, 36.252277);
            case 27: return new City("Каменск-Уральский", 56.414897, 61.918905 );
            case 28: return new City("Кемерово", 55.359594, 86.08778100000001);
            case 29: return new City("Киев", 50.402395, 30.532690 );
            case 30: return new City("Киров", 54.079033, 34.323163);
            case 31: return new City("Комсомольск-на-Амуре", 50.54986, 137.007867 );
            case 32: return new City("Королев", 55.916229, 37.854467);
            case 33: return new City("Кострома", 57.767683, 40.926418);
            case 34: return new City("Краснодар", 45.023877, 38.970157);
            case 35: return new City("Красноярск", 56.008691, 92.870529);
            case 36: return new City("Курск", 51.730361, 36.192647);
            case 37: return new City("Липецк", 52.61022, 39.594719);
            case 38: return new City("Магнитогорск", 53.411677, 58.984415 );
            case 39: return new City("Махачкала", 42.984913, 47.504646);
            case 40: return new City("Минск", 53.906077, 27.554914 );
            case 41: return new City("Москва", 55.755773, 37.617761);
            case 42: return new City("Мурманск", 68.96956299999999, 33.07454);
            case 43: return new City("Набережные Челны", 55.743553, 52.39582 );
            case 44: return new City("Нижний Новгород", 56.323902, 44.002267);
            case 45: return new City("Нижний Тагил", 57.910144, 59.98132 );
            case 46: return new City("Новокузнецк", 53.786502, 87.155205);
            case 47: return new City("Новороссийск", 44.723489, 37.76866);
            case 48: return new City("Новосибирск", 55.028739, 82.90692799999999);
            case 49: return new City("Норильск", 69.349039, 88.201014);
            case 50: return new City("Омск", 54.989342, 73.368212 );
            case 51: return new City("Орел", 52.970306, 36.063514);
            case 52: return new City("Оренбург", 51.76806, 55.097449);
            case 53: return new City("Пенза", 53.194546, 45.019529 );
            case 54: return new City("Первоуральск", 56.908099, 59.942935 );
            case 55: return new City("Пермь", 58.004785, 56.237654);
            case 56: return new City("Прокопьевск", 53.895355, 86.744657 );
            case 57: return new City("Псков", 57.819365, 28.331786);
            case 58: return new City("Ростов-на-Дону", 47.227151, 39.744972);
            case 59: return new City("Рыбинск", 58.13853, 38.573586);
            case 60: return new City("Рязань", 54.619886, 39.744954);
            case 61: return new City("Самара", 53.195533, 50.101801 );
            case 62: return new City("Санкт-Петербург", 59.938806, 30.314278);
            case 63: return new City("Саратов", 51.531528, 46.03582 );
            case 64: return new City("Севастополь", 44.616649, 33.52536 );
            case 65: return new City("Северодвинск", 64.55818600000001, 39.82962);
            case 66: return new City("Северодвинск", 64.558186, 39.82962 );
            case 67: return new City("Симферополь", 44.952116, 34.102411 );
            case 68: return new City("Сочи", 43.581509, 39.722882);
            case 69: return new City("Ставрополь", 45.044502, 41.969065);
            case 70: return new City("Сухум", 43.015679, 41.025071 );
            case 71: return new City("Тамбов", 52.721246, 41.452238);
            case 72: return new City("Ташкент", 41.314321, 69.267295 );
            case 73: return new City("Тверь", 56.859611, 35.911896);
            case 74: return new City("Тольятти", 53.511311, 49.418084);
            case 75: return new City("Томск", 56.495116, 84.972128);
            case 76: return new City("Тула", 54.193033, 37.617752);
            case 77: return new City("Тюмень", 57.153033, 65.534328 );
            case 78: return new City("Улан-Удэ", 51.833507, 107.584125);
            case 79: return new City("Ульяновск", 54.317002, 48.402243 );
            case 80: return new City("Уфа", 54.734768, 55.957838 );
            case 81: return new City("Хабаровск", 48.472584, 135.057732 );
            case 82: return new City("Харьков", 49.993499, 36.230376 );
            case 83: return new City("Чебоксары", 56.1439, 47.248887);
            case 84: return new City("Челябинск", 55.159774, 61.402455);
            case 85: return new City("Шахты", 47.708485, 40.215958);
            case 86: return new City("Энгельс", 51.498891, 46.125121 );
            case 87: return new City("Южно-Сахалинск", 46.959118, 142.738068 );
            case 88: return new City("Якутск", 62.027833, 129.704151);
            case 89: return new City("Ярославль", 57.626569, 39.893822);
        }
        throw new IndexOutOfBoundsException("Position " + position
                + " is out of bounds [" + 0 + "," + COUNT + ")");
    }

    private LargeRussianCities() {}
}
