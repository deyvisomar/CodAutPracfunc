#
library(reticulate)
library(tidyverse)
ruta<- "C:/Users/Deyvis/AppData/Local/Programs/Python/Python313/python.exe"
use_python(ruta)
## para enviar correo
py_run_string('
import win32com.client as win32

def enviar_correo_outlook_win32(destinatario_email, asunto, mensaje):
    try:
        outlook = win32.Dispatch("outlook.application")
        correo = outlook.CreateItem(0)  # 0 representa un nuevo correo electrónico
        correo.To = destinatario_email
        correo.Subject = asunto
        correo.Body = mensaje
        correo.Send()
        print("Correo electrónico enviado con éxito")
    except Exception as e:
        print(f"Error al enviar el correo electrónico: {e}")

# Ejemplo de uso
destinatario_email = "genewof752@evluence.com"  # Reemplaza con el destinatario
asunto = "Correo de prueba desde Python (win32com)"
mensaje = "Este es un correo de prueba enviado desde Python usando win32com. 17:20"

# Enviar el correo
enviar_correo_outlook_win32(destinatario_email, asunto, mensaje)
')

## para ver bandeja de entrada
py_run_string(
  ' 
  # Obtener la bandeja de entrada
import win32com.client

# Conectar a Outlook
outlook = win32com.client.Dispatch("Outlook.Application")
namespace = outlook.GetNamespace("MAPI")

# Obtener la bandeja de entrada
inbox = namespace.GetDefaultFolder(6)  # 6 corresponde a la Bandeja de Entrada
messages = inbox.Items

# Ordenar los mensajes por fecha de recepción (más reciente primero)
messages.Sort("[ReceivedTime]", True)

# Crear listas vacías para almacenar los detalles de los correos
subjects = []
senders = []
received_times = []
bodies = []

# Iterar sobre los correos y extraer la información
for message in messages:
    try:
        subjects.append(message.Subject)
        senders.append(message.SenderName)
        received_times.append(message.ReceivedTime)
        bodies.append(message.Body)
    except Exception as e:
        # Si no se puede leer un mensaje, se maneja el error
        print(f"Error al procesar el mensaje: {e}")
        continue

# Imprimir los vectores (listas)
#print("Asuntos:", subjects)
#print("Remitentes:", senders)
#print("Fechas de Recepción:", received_times)
print("Cuerpos:", bodies)
  '
)
#personas que enviaron el correo
py$senders
# fechas y hora de los correos
fechas<-as.POSIXct(unlist(py$received_times), format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
#
bodi<- gsub("[\n\r\t]", "", py$bodies)
cadenas_limpias <- paste(gsub("(<).*$", "\\1", bodi),"Enviado a la hora:", fechas)
#posicion <- str_locate(bodi, "El dom")[1]
# Extrae la cadena
#bodi <- str_sub(cadena, 1, posicion + 5)




library(clipr)

convertir_python_para_r <- function() {
  # Leer desde el portapapeles
  lineas <- readLines("clipboard")
  
  # Escapar comillas dobles y backslashes
  lineas_escapadas <- gsub("\\\\", "\\\\\\\\", lineas)        # \  → \\
  lineas_escapadas <- gsub("\"", "\\\\\"", lineas_escapadas)  # "  → \"
  
  # Agregar comillas a cada línea
  lineas_citadas <- paste0('"', lineas_escapadas, '"')
  
  # Armar todo el bloque del paste
  salida <- c(
    "codigo_python <- paste(",
    paste0("  ", lineas_citadas, collapse = ",\n"),
    '  , sep = "\\n")'
  )
  
  # Copiar el resultado al portapapeles
  write_clip(salida)
  
  cat("✅ Código convertido y copiado al portapapeles.\n¡Pegalo en tu script R y ejecutá!\n")

  #cat(salida, "\n")
  }
convertir_python_para_r()

codigo_python <- paste(
  "import os",
  "import win32com.client",
  "",
  "# Iniciar Outlook",
  "outlook = win32com.client.Dispatch(\"Outlook.Application\").GetNamespace(\"MAPI\")",
  "",
  "# -------- ACCESO A BANDEJA DE ENTRADA (RECIBIDOS) --------",
  "bandeja_entrada = outlook.GetDefaultFolder(6)",
  "mensajes_recibidos = bandeja_entrada.Items",
  "mensajes_recibidos.Sort(\"[ReceivedTime]\", True)",
  "",
  "# Seleccionamos un correo de referencia para obtener el ConversationID",
  "correo_base = mensajes_recibidos.Item(1)",
  "conversation_id = correo_base.ConversationID",
  "print(f\"\\nAnalizando hilo con ConversationID: {conversation_id}\")",
  "",
  "# -------- ACCESO A ENVIADOS DE LA CUENTA dmamani --------",
  "cuenta = outlook.Folders[\"deyvisomarmq1@outlook.com\"]",
  "carpeta_enviados = cuenta.Folders[\"Elementos enviados\"]",
  "mensajes_enviados = carpeta_enviados.Items",
  "mensajes_enviados.Sort(\"[SentOn]\", True)",
  "",
  "# -------- FILTRAR LOS CORREOS RELACIONADOS AL HILO --------",
  "correos_relacionados = []",
  "",
  "# Recibidos",
  "for mensaje in mensajes_recibidos:",
  "    try:",
  "        if mensaje.Class == 43 and mensaje.ConversationID == conversation_id:",
  "            correos_relacionados.append({",
  "                \"tipo\": \"recibido\",",
  "                \"asunto\": mensaje.Subject,",
  "                \"remitente\": mensaje.SenderName,",
  "                \"destinatario\": mensaje.To,",
  "                \"fecha\": mensaje.ReceivedTime,",
  "                \"cuerpo\": mensaje.Body,",
  "                \"adjuntos\": mensaje.Attachments",
  "            })",
  "    except:",
  "        pass",
  "",
  "# Enviados",
  "for mensaje in mensajes_enviados:",
  "    try:",
  "        if mensaje.Class == 43 and mensaje.ConversationID == conversation_id:",
  "            correos_relacionados.append({",
  "                \"tipo\": \"enviado2\",",
  "                \"asunto\": mensaje.Subject,",
  "                \"remitente\": mensaje.SenderName,",
  "                \"destinatario\": mensaje.To,",
  "                \"fecha\": mensaje.SentOn,",
  "                \"cuerpo\": mensaje.Body,",
  "                \"adjuntos\": mensaje.Attachments",
  "            })",
  "    except:",
  "        pass",
  "",
  "# -------- ORDENAR LOS CORREOS POR FECHA --------",
  "correos_relacionados.sort(key=lambda x: x[\"fecha\"])",
  "",
  "# -------- MOSTRAR LA CONVERSACIÓN COMPLETA --------",
  "print(f\"\\nConversación completa ({len(correos_relacionados)} correos encontrados):\\n\")",
  "",
  "# Crear una carpeta para guardar los adjuntos (si no existe)",
  "carpeta_adjuntos = \"adjuntos\"",
  "if not os.path.exists(carpeta_adjuntos):",
  "    os.makedirs(carpeta_adjuntos)",
  "",
  "# Mostrar detalles de cada correo",
  "for i, correo in enumerate(correos_relacionados, 1):",
  "    print(f\"\\n--- Correo #{i} ---\")",
  "    print(f\"Tipo: {\"Recibido\" if correo[\"tipo\"] == \"recibido\" else \"Enviado\"}\")",
  "    print(f\"Asunto: {correo[\"asunto\"]}\")",
  "    print(f\"De: {correo[\"remitente\"]}\")",
  "    print(f\"Para: {correo[\"destinatario\"]}\")",
  "    print(f\"Fecha: {correo[\"fecha\"]}\")",
  "    print(\"Cuerpo:\")",
  "    print(correo[\"cuerpo\"][:500])  # Solo muestra los primeros 500 caracteres",
  "    print(\"-\" * 60)",
  "    ",
  "    # Verificar y mostrar los archivos adjuntos",
  "    if correo[\"adjuntos\"].Count > 0:",
  "        print(\"\\nArchivos adjuntos:\")",
  "        for j in range(1, correo[\"adjuntos\"].Count + 1):",
  "            adjunto = correo[\"adjuntos\"].Item(j)",
  "            archivo_nombre = adjunto.FileName",
  "            print(f\"  - {archivo_nombre}\")",
  "            ",
  "            # Guardar el adjunto en la carpeta local",
  "          #  adjunto.SaveAsFile(os.path.join(carpeta_adjuntos, archivo_nombre))",
  "        print(\"-\" * 60)"
  , sep = "\n")


py_run_string(codigo_python)









