%% Import Data
clear all
clc

maxfort = 12;
m = cell(1, maxfort);

for n = 10:maxfort
    filename = sprintf('fort.%d', n);
    m{n} = importdata(filename);
end
con = importdata('connectivity.txt');
nquad = size(con(:,1));
colormap jet
%% Plot and make movie
for n = 10:maxfort;
    coord = [m{1,n}.data(1:38550,1) m{1,n}.data(1:38550,2) m{1,n}.data(1:38550,3) m{1,n}.data(1:38550,4)];
    for i = 1:38144
    X(i, 1:4) = [coord(con(i,1),1), coord(con(i,2),1), coord(con(i,3),1), coord(con(i,4),1)];
    Y(i, 1:4) = [coord(con(i,1),2), coord(con(i,2),2), coord(con(i,3),2), coord(con(i,4),2)];
    C(i, 1:4) = [coord(con(i,1),4), coord(con(i,2),4), coord(con(i,3),4), coord(con(i,4),4)];
    end
    colorbar
    caxis([-50, 50])
    patch(X',Y',C', 'linestyle', 'None')
    axis([-1 3 -2 2])
    drawnow
    M(n-9) = getframe(gcf);
    clf
end
movie2avi(M, 'StructuresK1Transpose')


