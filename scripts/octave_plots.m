load('../data/cleaned/data.mat')
years = str2num(cell2mat(x(:,1)));
eighty = x((years == 1980),:);
ninety = x((years == 1990),:);
two = x((years == 2000),:);

d1 = setdiff(eighty(:,2),two(:,2));
d2 = setdiff(two(:,2),eighty(:,2));
d3 = setdiff(ninety(:,2),eighty(:,2));
d4 = setdiff(eighty(:,2),ninety(:,2));
d5 = setdiff(two(:,2),ninety(:,2));
d6 = setdiff(ninety(:,2),two(:,2));

diff = [d1; d2; d3; d4; d5; d6];

eighty_trim = eighty(~ismember(eighty(:,2),diff),:);
two_trim = two(~ismember(two(:,2),diff),:);
ninety_trim = ninety(~ismember(ninety(:,2),diff),:);

etr = eighty_trim(find(not(cellfun('isempty', strfind(eighty_trim(:,3), 'Retail')))),:);
eth = eighty_trim(find(not(cellfun('isempty', strfind(eighty_trim(:,3), 'Hightech')))),:);
etm = eighty_trim(find(not(cellfun('isempty', strfind(eighty_trim(:,3), 'Manufacturing')))),:);

ntr = ninety_trim(find(not(cellfun('isempty', strfind(ninety_trim(:,3), 'Retail')))),:);
nth = ninety_trim(find(not(cellfun('isempty', strfind(ninety_trim(:,3), 'Hightech')))),:);
ntm = ninety_trim(find(not(cellfun('isempty', strfind(ninety_trim(:,3), 'Manufacturing')))),:);

ttr = two_trim(find(not(cellfun('isempty', strfind(two_trim(:,3), 'Retail')))),:);
tth = two_trim(find(not(cellfun('isempty', strfind(two_trim(:,3), 'Hightech')))),:);
ttm = two_trim(find(not(cellfun('isempty', strfind(two_trim(:,3), 'Manufacturing')))),:);

jer = str2num(cell2mat(etr(:,4)));
jeh = str2num(cell2mat(eth(:,4)));
jem = str2num(cell2mat(etm(:,4)));

jnr = str2num(cell2mat(ntr(:,4)));
jnh = str2num(cell2mat(nth(:,4)));
jnm = str2num(cell2mat(ntm(:,4)));

jtr = str2num(cell2mat(ttr(:,4)));
jth = str2num(cell2mat(tth(:,4)));
jtm = str2num(cell2mat(ttm(:,4)));


figure1 = figure;
axes1 = axes('Parent',figure1);
hold(axes1,'all');
scatter(1:219, (jnr-jer)./jer)
line(0,0)
axis([0 250 -2 3])
title('Percent growth 80 to 90 vs city in retail')
saveas(figure1, '../graphs/8090R.jpg')

figure1 = figure;
axes1 = axes('Parent',figure1);
hold(axes1,'all');
scatter(1:219, (jnh - jeh)./jeh)
line(0,0)
axis([0 250 -2 3])
title('Percent growth 80 to 90 vs city in hightech')
saveas(figure1, '../graphs/8090H.jpg')

figure1 = figure;
axes1 = axes('Parent',figure1);
hold(axes1,'all');
scatter(1:219, (jnm - jem)./jem)
line(0,0)
axis([0 250 -2 3])
title('Percent growth 80 to 90 vs city in manufacturing')
saveas(figure1, '../graphs/8090M.jpg')

figure1 = figure;
axes1 = axes('Parent',figure1);
hold(axes1,'all');
scatter(1:219, (jtr-jnr)./jnr)
line(0,0)
axis([0 250 -2 3])
title('Percent growth 90 to 00 vs city in retail')
saveas(figure1, '../graphs/9000R.jpg')

figure1 = figure;
axes1 = axes('Parent',figure1);
hold(axes1,'all');
scatter(1:219, (jth - jnh)./jnh)
line(0,0)
axis([0 250 -2 3])
title('Percent growth 90 to 00 vs city in hightech')
saveas(figure1, '../graphs/9000H.jpg')

figure1 = figure;
axes1 = axes('Parent',figure1);
hold(axes1,'all');
scatter(1:219, (jtm - jnm)./jnm)
line(0,0)
axis([0 250 -2 3])
title('Percent growth 90 to 00 vs city in manufacturing')
saveas(figure1, '../graphs/9000M.jpg')

disp('median for percent growth 80 to 90 vs city in retail')
median((jnr-jer)./jer)
disp('median for percent growth 80 to 90 vs city in hightech')
median((jnh - jeh)./jeh)
disp('median for percent growth 80 to 90 vs city in manufacturing')
median((jnm - jem)./jem)

disp('median for percent growth 90 to 00 vs city in retail')
median((jtr-jnr)./jnr)
disp('median for percent growth 90 to 00 vs city in hightech')
median((jth - jnh)./jnh)
disp('median for percent growth 90 to 00 vs city in manufacturing')
median((jtm - jnm)./jnm)
