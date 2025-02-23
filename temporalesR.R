server <- function(input, output) {
  observeEvent(input$convertFileButton, {
    apiKey <- input$apiKey
    audioFile <- input$audioFile
    
    if (!is.null(audioFile)) {
      if (!is_api_key_valid(apiKey, output)) return()
      
      process_audio_and_transcribe(audioFile$datapath, apiKey, output, encoding = "WEBM_OPUS")
    } else {
      handle_audio_file_selection_error(output)
    }
  })
  
  # MODIFICADO: El evento ahora reacciona a la actualización de 'recordedAudioBase64'
  observeEvent(input$recordedAudioBase64, {
    recordedAudioBase64 <- input$recordedAudioBase64 # Obtiene el audio grabado en base64
    apiKey <- input$apiKey
    
    # Solo procede si recordedAudioBase64 no es NULL (es decir, si hay audio grabado)
    if (!is.null(recordedAudioBase64)) {
      if (!is_api_key_valid(apiKey, output)) return()
      
      # Decodifica la cadena base64 a datos binarios raw
      recordedAudioData <- base64decode(recordedAudioBase64)
      
      # Procesa los datos de audio grabados y transcribe
      process_audio_data_and_transcribe(recordedAudioData, apiKey, output, encoding = "WEBM_OPUS")
    }
    # No es necesario manejar el caso NULL aquí, ya que el botón 'Convertir Audio Grabado' siempre enviará algo (o NULL si no hay grabación, que se maneja en JS para no enviar si no hay blob)
  })
  
  
  # Funciones reutilizadas (sin cambios desde la versión anterior)
  is_api_key_valid <- function(apiKey, output) {
    if (apiKey == "TU_API_KEY" || nchar(apiKey) == 0) {
      output$error <- renderText("Por favor, ingresa tu API Key de Google Cloud en el campo de texto.")
      output$transcription <- renderText(NULL)
      output$request_json <- renderText(NULL)
      return(FALSE)
    }
    TRUE
  }
  
  handle_audio_file_selection_error <- function(output) {
    output$transcription <- renderText("Por favor, selecciona un archivo de audio.")
    output$error <- renderText(NULL)
    output$request_json <- renderText(NULL)
  }
  
  
  process_audio_and_transcribe <- function(audio_path, apiKey, output, encoding) {
    output$error <- renderText(NULL)
    output$transcription <- renderText("Procesando audio, por favor espera...")
    output$request_json <- renderText(NULL)
    
    tryCatch({
      audioData <- readBin(audio_path, what = "raw", n = file.info(audio_path)$size)
      transcribe_audio_data(audioData, apiKey, output, encoding)
      
    }, error = function(e) {
      handle_transcription_error(e, output)
    })
  }
  
  
  process_audio_data_and_transcribe <- function(audioData, apiKey, output, encoding) {
    output$error <- renderText(NULL)
    output$transcription <- renderText("Procesando audio grabado, por favor espera...")
    output$request_json <- renderText(NULL)
    
    tryCatch({
      transcribe_audio_data(audioData, apiKey, output, encoding)
      
    }, error = function(e) {
      handle_transcription_error(e, output)
    })
  }
  
  
  transcribe_audio_data <- function(audioData, apiKey, output, encoding) {
    url <- "https://speech.googleapis.com/v1/speech:recognize"
    body <- list(
      config = list(
        encoding = encoding,
        sampleRateHertz = 48000,
        languageCode = "es-ES"
      ),
      audio = list(
        content = base64enc::base64encode(audioData)
      )
    )
    
    output$request_json <- renderText(toJSON(body, auto_unbox = TRUE, pretty = TRUE))
    
    response <- POST(
      url,
      query = list(key = apiKey),
      body = toJSON(body, auto_unbox = TRUE),
      content_type_json()
    )
    
    if (http_status(response)$category != "Success") {
      stop(paste("Error en la solicitud a la API:", http_status(response)$message, "- Detalles adicionales (si están disponibles): ", content(response, "text")))
    }
    
    result <- fromJSON(content(response, "text"), simplifyVector = FALSE)
    
    transcription <- extract_transcription_from_result(result)
    output$transcription <- renderText(transcription)
  }
  
  extract_transcription_from_result <- function(result) {
    if (!is.null(result$results) && length(result$results) > 0 &&
        !is.null(result$results[[1]]$alternatives) && length(result$results[[1]]$alternatives) > 0) {
      result$results[[1]]$alternatives[[1]]$transcript
    } else {
      "No se pudo transcribir el audio. Intenta con otro archivo o verifica la configuración."
    }
  }
  
  handle_transcription_error <- function(e, output) {
    print(e)
    output$transcription <- renderText(NULL)
    output$request_json <- renderText(NULL)
    output$error <- renderText(paste("Ocurrió un error durante la transcripción:", e$message))
  }
}