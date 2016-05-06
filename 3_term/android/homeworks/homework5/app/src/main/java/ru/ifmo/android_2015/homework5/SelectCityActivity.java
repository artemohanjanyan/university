package ru.ifmo.android_2015.homework5;

import android.content.Intent;
import android.graphics.Color;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.util.Log;

import ru.ifmo.android_2015.homework5.list.CitiesRecyclerAdapter;
import ru.ifmo.android_2015.homework5.list.CitySelectedListener;
import ru.ifmo.android_2015.homework5.list.RecyclerDividersDecorator;
import ru.ifmo.android_2015.homework5.model.City;

public class SelectCityActivity extends AppCompatActivity
        implements CitySelectedListener {

    // Прокручивающийся список городов
    private RecyclerView recyclerView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_select_city);
        recyclerView = (RecyclerView) findViewById(R.id.list);
        recyclerView.setLayoutManager(new LinearLayoutManager(this));
        recyclerView.addItemDecoration(new RecyclerDividersDecorator(Color.DKGRAY));
        CitiesRecyclerAdapter adapter = new CitiesRecyclerAdapter(this);
        adapter.setCitySelectedListener(this);
        recyclerView.setAdapter(adapter);
    }

    @Override
    public void onCitySelected(City city) {
        Log.i(TAG, "onCitySelected: " + city);
        // Запускаем экран CityCamActivity, который покажет веб-камеру из выбранного города
        Intent cityCam = new Intent(this, CityCamActivity.class);
        cityCam.putExtra(CityCamActivity.EXTRA_CITY, city);
        startActivity(cityCam);
    }

    private static final String TAG = "SelectCity";

}
