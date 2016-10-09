package ru.ifmo.rain.ohanjanyan

import com.google.api.services.sheets.v4.Sheets

fun main(args: Array<String>) {
    val service: Sheets = SheetsQuickstart.getSheetsService()

    val resultsId = "1AhWWojdTe3jStaGxwic2NI2zDuc36vQs7MuPZl6sz30"
    val oldInfoId = "1RsLVUR5zEvVfqkurOTd35V1byS2M8MUt1bKmel13lHs"
    val newInfoId = "1IN2aZgXD_5Jr0HTQI4CEtZU6nwKMz4CYzH49l4ND6JQ"
    val ticketsId = "1acGfd2XLnB873zoXuWHkH551StJ6ThO1XdJ7Vqbn7NA"

    val infoProvider = SheetStudentInfoProvider(service, oldInfoId, newInfoId)
    val resultsProvider = SheetsResultsProvider(service, resultsId, infoProvider)
    val ticketService = SheetTicketService(service, ticketsId, resultsProvider, infoProvider)
    for (group in 39 downTo 36) {
        ticketService.makeTicketPage(group, 61 until 72)
    }
}