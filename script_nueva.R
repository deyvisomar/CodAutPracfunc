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









