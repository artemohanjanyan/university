package ru.ifmo.rain.ohanjanyan

import com.google.api.services.sheets.v4.Sheets

fun main(args: Array<String>) {
    println("Kotlin works")

    val service: Sheets = SheetsQuickstart.getSheetsService()

    val spreadsheet = service.spreadsheets()
            .get("1hfID1l8iafWZVMmFoDaHBNeCPmdCIjeil9_6VeoroIA")
            .setIncludeGridData(true)
            .execute()

    val cellData = spreadsheet.sheets[0].data[0].rowData[4].getValues()[4]
    println(cellData)
    println(cellData)

//    val resultsSpreadsheet = service.spreadsheets()
//            .get("1aaTO-dAZRJ162vh94KUKgCeAPRvNaYm3QYEkika9MCc")
//            .execute()
//    val resultSheet = resultsSpreadsheet.sheets[0]
//    val groupNumbers = service.spreadsheets().values().get(
//            resultsSpreadsheet.spreadsheetId,
//            resultSheet.properties.title + "!A5:B110"
//    ).execute()
//    val groupNumberByName = groupNumbers.getValues().associateBy({ it[0] }, { it[1] })
//    println(groupNumberByName)

//    val resultsSpreadsheet = service.spreadsheets()
//            .get("1aaTO-dAZRJ162vh94KUKgCeAPRvNaYm3QYEkika9MCc")
//            .execute()
//    val resultSheet = resultsSpreadsheet.sheets[0]
//    val groupNumbers = service.spreadsheets().values().get(
//            resultsSpreadsheet.spreadsheetId,
//            resultSheet.properties.title + "!B2:D36"
//    ).execute()
//    val groupNumberByEmail = groupNumbers.getValues().associateBy({ it[2] }, { it[0] })
//    println(groupNumberByEmail)

//    val listSpreadsheet = service.spreadsheets()
//            .get("1RsLVUR5zEvVfqkurOTd35V1byS2M8MUt1bKmel13lHs")
//            .execute()
//    //val listSheet = listSpreadsheet.sheets.first()
//    val studentsInfo = service.spreadsheets().values().get(
//            listSpreadsheet.spreadsheetId,
//            "A1:C59"
//    ).execute()
//    println(studentsInfo)

//    studentsInfo.getValues().forEach {
//        val group = groupNumberByEmail[it[1]]
//        if (it.size == 3) {
//            it[2] = group
//        } else {
//            it.add(2, group)
//        }
////    }
//    studentsInfo.getValues().forEach {
//        val group = groupNumberByName[it[0]]
//        it[2] = group
//    }
//
//    println(service.spreadsheets().values()
//            .update(
//                    listSpreadsheet.spreadsheetId,
//                    studentsInfo.range,
//                    studentsInfo)
//            .setValueInputOption("RAW")
//            .execute())
}
