package ru.ifmo.android_2015.citycam.list;

import ru.ifmo.android_2015.citycam.model.City;

/**
 * Интерфейс дял получения событий от списка городов.
 */
public interface CitySelectedListener {
    /**
     * Вызывается, когда указанный город был выбран в списке городов.
     */
    void onCitySelected(City city);
}
