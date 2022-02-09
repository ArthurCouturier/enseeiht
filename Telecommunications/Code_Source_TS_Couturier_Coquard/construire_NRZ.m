function NRZ = construire_NRZ(length, Ns, bin)
NRZ = ones(1, length*Ns);
for k = 1:length
    NRZ(1,Ns*(k-1)+1:Ns*k) = bin(k) * NRZ(1,Ns*(k-1)+1:Ns*k);
end


% NRZ = zeros(1, length*Ns-1);
% for i = 1:length
%     NRZ((i-1)*Ns+1:i*Ns) = bin(i) * NRZ((i-1)*Ns+1:i*Ns);
% end