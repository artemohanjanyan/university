# Домашнее задание 2

## Цель

Написать приложение City Cam, которое предлагает пользователю выбрать город из списка и показывает статичное изображение с веб-камеры в выбранном городе. Код этого домашнего задания уже содержит готовый класс SelectCityActivity, который показывает список городов и осуществляет выбор одного из них. После того, как город выбран, запускается второй экран -- CityCamActivity, который должен получить информацию о веб-камерах по координатам выбранного города при помощи Webcams.Travel API (http://www.webcams.travel/developers/introduction), загрузить изображение с одной из камер и показать это изображение в UI вместе с подробной информацией о веб-камере.

## Задание

* Зарегистрироваться на http://www.webcams.travel/developers и получить ID разработчика
* Вставить значение ID разработчика в код класса Webcams (см. комментарии)
* Ознакомиться с API методом получения информации о камерах по координатам: http://www.webcams.travel/developers/api/wct.webcams.list_nearby -- и выяснить, какую информацию из него можно извлечь. Как минимум, нужна статичная картинка с камеры (preview_url). Дополнительная информация приветствуется (название камеры, место, время)
* Написать AsyncTask, который запускается из CityCamActivity.onCreate() и выполняет следующие действия:
  * Выполняет запрос wct.webcams.list_nearby при помощи HttpURLConnection. (Есть готовый метод Webcams.createNearbyUrl() для получения URL этого запроса)
  * Извлекает информацию из ответа в формате JSON при помощи JsonReader 
  * Скачивает статичную фотографию 
  * Декодирует фотографию при помощи BitmapFactory.decodeStream() и получает объект Bitmap
* Получить результат выполнения этого AsyncTask в CityCamActivity и показать полученное изображение и дополнительную информацию о камере в UI
* Для отображения дополнительной информации может потребовать добавить что-нибудь в верстку: res/layout/activity_city_cam.xml

## Примечание

В отличие от лекции, где использовались методы onRetainNonConfigurationInstance() и getLastNonConfigurationInstance(), в коде CityCamActivity для той же цели следует использовать методы onRetainCustomNonConfigurationInstance() и getLastCustomNonConfigurationInstance(). Это вызвано тем, что у CityCamActivity базовым классом является FragmentActivity, в котором первые методы объявлены final. Кто хочет, может использовать фрагменты. 

## Требования

* При повороте экрана НЕ должна происходить повторная загрузка данных
* Долгие операции НЕ должны выполняться в UI потоке

## Порядок сдачи

Сдавать задание нужно в виде форка и пулл-реквеста к https://github.com/IFMO-Android-2015/homework2, в описании укажите ФИО. Подробнее про пулл-реквесты можно почитать здесь: http://habrahabr.ru/post/125999/ и здесь: https://help.github.com/articles/using-pull-requests.

## Полезные ссылки

* http://developer.android.com/reference/android/graphics/BitmapFactory.html#decodeStream(java.io.InputStream)
* http://developer.android.com/reference/android/util/JsonReader.html
* http://developer.android.com/reference/android/os/AsyncTask.html
* http://developer.android.com/training/basics/network-ops/connecting.html


## Лекция №2

https://github.com/IFMO-Android-2015/lesson2



  
