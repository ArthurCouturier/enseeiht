donnees;
close all;


%% 3.1.1 NRZ
NRZ = construire_NRZ(n, Ns, signalBin);

f = linspace(-Fe/2,Fe/2, length(NRZ)); 
Sx = Ts/4 * sinc(pi*f*Ts);
Sx(Ns*n/2) = Sx(Ns*n/2) + 1/4;

figure(1);
plot(f, Sx);
xlabel('Frequence (Hz)');

% Affichage inclus dans la figure de résultat 3.4


%% 3.1.2 Génération du signal modulé en fréquence
x = (1-NRZ).*cos(t*2*pi*F0 + phi0) + NRZ.*cos(t*2*pi*F1 + phi1);

figure(2)
plot(t, x)
title('Signal modulé en fréquence');
xlabel('Temps (s)');
ylabel('Amplitude');
ylim([-1.2, 1.2]);

Snrz = abs(fft(x));

figure(3);
plot(linspace(-Fe/2,Fe/2,length(Snrz)), fftshift(Snrz));
xlabel('Frequence (Hz)');
legend( 'réponse en fréquence du signal d''origine')


%% 3.2 Canal de transmission à bruit additif, blanc et Gaussien
Px = mean(abs(x).^2);
P = Px*10^(-SNRdB/10);   
noise = sqrt(P)*randn(1,Ns*n);
x_noise = x+noise;
% Affichage inclus dans la figure de résultat 3.4


%% 3.3.1 Synthèse du filtre passe-bas
% Synthèse filtre passe haut
h_pb = 2*fc*Te*sinc(2*fc*Te*N);
H_pb = fft(h_pb);

%% 3.3.2 Synthèse du filtre passe-haut
h_ph = -h_pb;   
h_ph((ordre-1)/2+1) = h_ph((ordre-1)/2+1) +1;
H_ph = fft(h_ph);


%% 3.3.3  Filtrage
% on aggrandi le signal à filtrer pour pouvoir ensuite compenser le 
% décalage de (ordre -1)/2 sample induit par le filte en fonction de son
% ordre
x_buffered = [x_noise zeros(1,(ordre-1)/2)]; 

% Passe haut
x_pb = filter(h_pb,1,x_buffered);
x_pb = x_pb((ordre-1)/2+1:end);

%Passe bas
x_ph = filter(h_ph,1,x_buffered);
x_ph = x_ph((ordre-1)/2+1:end);


%% 3.3.4  Tracé à réaliser
figure(4);
plot(linspace(-Fe/2,Fe/2,length(N)),fftshift(abs(H_pb)),'blue', linspace(-Fe/2,Fe/2,length(N)),fftshift(abs(H_ph)),'red');
legend('réponse en fréquence passe bas', 'réponse en fréquence passe bas');
xlabel('Frequence (Hz)');

figure(5);
plot(linspace(-Te/2,Te/2,length(N)),h_pb,'blue', linspace(-Te/2,Te/2,length(N)),h_ph,'red');
legend('réponse en fréquence passe bas', 'réponse en fréquence passe bas');
xlabel('Temps (s)');

figure(6);
Rx_pb = xcorr(x_pb,'unbiased');
dsp_pb = abs(fft(Rx_pb));
Rx_ph = xcorr(x_ph,'unbiased');
dsp_ph = abs(fft(Rx_ph));
plot(linspace(-Fe/2,Fe/2,length(dsp_pb)), fftshift(dsp_pb/max([dsp_pb dsp_ph])), 'blue', linspace(-Fe/2,Fe/2,length(dsp_ph)), fftshift(dsp_ph/max([dsp_pb dsp_ph])), 'red', linspace(-Fe/2,Fe/2,length(N)),fftshift(abs(H_pb)),'--b', linspace(-Fe/2,Fe/2,length(N)),fftshift(abs(H_ph)),'--r');
legend('DSP signal filtré passe bas (normalisée)', 'DSP signal filtré passe haut (normalisée)', 'réponse en fréquence passe bas', 'réponse en fréquence passe bas');
xlabel('Frequence (Hz)');
ylabel('Amplitude');

%% 3.3.5 Detection d'energie
% Test pour determiner le meilleur seuil, dans notre cas K=30 (a
% décommenter pour afficher une estimation à 10e-2 près du meilleur K pour
% un signal donné)
%Kmax = estimer_seuil_experimental(n, Ns, x_pb, signalBin) 

signal_result = zeros(1,n);

for i = 1:n
    signal_result(i) = sum(x_pb(Ns*(i-1)+1:Ns*i).^2) >K; 
end
    
Taux_erreur_filtrage = sum(signalBin ~= signal_result)/n

% Afficher les résultats
NRZ_sortie = construire_NRZ(n, Ns, signal_result);
afficher_resultats(t, x_noise, x_pb, x_ph, NRZ, NRZ_sortie, '3.4: Demodulation par filtrage')

% Si le signal binaire est de la bonne taille, reconsitué l'image
% (si cette fonction est utilisé sur un signal aléatoire, l'image ne sera
% recomposé que de bruit)
if n == 84000
    reconstitution_image(signal_result) ;
    which reconstitution_image ;
end