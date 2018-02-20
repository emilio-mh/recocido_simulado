#include <stdlib.h>
#include <stdio.h>

int main(){
    FILE* cancion;
    cancion = fopen("cancion1.mp3", "rb");
    char* clave = "hola";
    long n;
    fseek(cancion, 0, SEEK_END);
    n = ftell(cancion);
    rewind(cancion);
    char* bff = (char*)malloc(sizeof(char)*n);
    fread(bff, 1, n, cancion);
    fclose(cancion);
    int i = 0;
    while(i < n){
        *(bff + i) = *(bff + i) ^ *(clave + (i%4));
        i++;
    }
    i = 0;
    cancion = fopen("cifrado1", "wb");
    fwrite(bff, 1, n, cancion);
    fclose(cancion);

    FILE* llave;
    llave = fopen("llave", "rb");
    fseek(llave, 0, SEEK_END);
    n = ftell(llave);
    rewind(llave);
    clave = (char*)malloc(sizeof(char)*n);
    fread(clave, 1, n, llave);
    fclose(llave);


    
    cancion = fopen("cancion2.mp3", "rb");
    long m;
    fseek(cancion, 0, SEEK_END);
    m = ftell(cancion);
    rewind(cancion);
    char *bfff = (char *)malloc(sizeof(char) * m);
    fread(bfff, 1, m, cancion);
    fclose(cancion);
    i = 0;
    while (i < m)
    {
        *(bfff + i) = *(bfff + i) ^ *(clave + (i % n));
        i++;
    }
    i = 0;
    cancion = fopen("cifrado2", "wb");
    fwrite(bfff, 1, n, cancion);
    fclose(cancion);

    while(i < n){
        *(bff + i) = *(bff + i) ^ *(bfff + i);
        i++;
    }
    cancion = fopen("rolon.mp3", "wb");
    fwrite(bff, 1, n, cancion),
    fclose(cancion);



}