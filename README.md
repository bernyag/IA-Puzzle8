# IA-Puzzle8
## Integrantes
- Bernardo Altamirano (167881)
- Eduardo Pesqueira Zorrilla (176065)
- Ian Zaidenweber (176705)
- Antonino Garcia (180164)

## Objetivo
Un puzzle-8 es un juego de una matriz de 3x3 en donde hay un espacio en blanco (o un 0) y números del 1 al 8 sin repetirse. El objetivo del juego es hacer movimientos (arriba, abajo, izquierda o derecha) con el cuadro en blanco hasta llegar a un estado meta.
En nuestro proyecto, se da un estado inicial dentro de la interfaz de Netbeans y se usa este para calcular los movimientos para llegar al estado meta de 0, 1, 2, 3, 4, 5, 6, 7, 8.

## Requerimientos
Para correr exitosamente el programa se requeiere `Java, Netbeans 12.2 y Clisp.` 
El programa está compuesto de dos partes: el frontend (java) y el backend (clisp). 
- El frontend está hecho en Java, utilizando Netbeans 12.2 para ejecutarla.
- El backend está hecho en common lisp (clisp), en donde se le da como parámetro el estado inicial y calcula la ruta utilizando la heurística de Manhattan.

## Manual de uso
1. Abrir el proyecto 8Puzzle en Java
3. Correr el proyecto
4. Elegir un estado inicial dentro de la GUI al hacer click en los cuadros e ingresar un dígito de 0 a 8 sin repetir
5. Hacer click en "Resolver", esto va a llamar el método de clisp para calcular la ruta y la va a regresar en el TextBox del GUI
6. Usar las flechas del GUI para hacer los movimientos hacia el estado final ( U = arriba, D = abajo, L = izquierda, R = derecha)

## Validaciones
Hay algunas validaciones para evitar que se trate de resolver un estado que no esté bien definido:
- No se puede repetir números
- No se puede poner números mayores a 8
- No pueden faltar números
