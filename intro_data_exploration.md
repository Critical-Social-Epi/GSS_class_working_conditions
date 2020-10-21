Intro data exploration
================
Jerzy Eisenberg-Guyot
10/21/2020

  - [Descriptives](#descriptives)
      - [Demographics and health stratifed by class (weighted but
        unimputed)](#demographics-and-health-stratifed-by-class-weighted-but-unimputed)
      - [QWL variables stratified by class (weighted but
        unimputed)](#qwl-variables-stratified-by-class-weighted-but-unimputed)

# Descriptives

Some QWL variables have lots of missingness because they weren’t asked
of every ballot in a given year

“depress” is whether respondent has ever been diagnosed by a healthcare
professional with depression. “mntlhlth” is days of poor mental health
in past 30 days

## Demographics and health stratifed by class (weighted but unimputed)

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<caption>

Simple

</caption>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

Workers

</th>

<th style="text-align:left;">

Managers

</th>

<th style="text-align:left;">

Petit bourgeoisie

</th>

<th style="text-align:left;">

Capitalists

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

n

</td>

<td style="text-align:left;">

4651.5

</td>

<td style="text-align:left;">

2575.5

</td>

<td style="text-align:left;">

664.2

</td>

<td style="text-align:left;">

478.8

</td>

</tr>

<tr>

<td style="text-align:left;">

sex = female (%)

</td>

<td style="text-align:left;">

2557.6 (55.0)

</td>

<td style="text-align:left;">

1191.7 (46.3)

</td>

<td style="text-align:left;">

327.3 (49.3)

</td>

<td style="text-align:left;">

120.9 (25.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

race (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

white

</td>

<td style="text-align:left;">

3290.9 (70.7)

</td>

<td style="text-align:left;">

1972.3 (76.6)

</td>

<td style="text-align:left;">

527.6 (79.4)

</td>

<td style="text-align:left;">

400.6 (83.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

black

</td>

<td style="text-align:left;">

729.5 (15.7)

</td>

<td style="text-align:left;">

307.7 (11.9)

</td>

<td style="text-align:left;">

46.8 (7.0)

</td>

<td style="text-align:left;">

26.4 (5.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

other

</td>

<td style="text-align:left;">

631.1 (13.6)

</td>

<td style="text-align:left;">

295.4 (11.5)

</td>

<td style="text-align:left;">

89.8 (13.5)

</td>

<td style="text-align:left;">

51.8 (10.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

educ (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

\<HS

</td>

<td style="text-align:left;">

517.2 (11.1)

</td>

<td style="text-align:left;">

195.0 (7.6)

</td>

<td style="text-align:left;">

87.0 (13.1)

</td>

<td style="text-align:left;">

37.7 (7.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

HS

</td>

<td style="text-align:left;">

2533.1 (54.5)

</td>

<td style="text-align:left;">

1154.7 (44.8)

</td>

<td style="text-align:left;">

317.8 (47.9)

</td>

<td style="text-align:left;">

194.6 (40.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

JuCo

</td>

<td style="text-align:left;">

405.3 (8.7)

</td>

<td style="text-align:left;">

260.8 (10.1)

</td>

<td style="text-align:left;">

56.5 (8.5)

</td>

<td style="text-align:left;">

36.9 (7.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

College +

</td>

<td style="text-align:left;">

1195.8 (25.7)

</td>

<td style="text-align:left;">

965.0 (37.5)

</td>

<td style="text-align:left;">

202.9 (30.5)

</td>

<td style="text-align:left;">

209.5 (43.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

marital\_tri (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

married

</td>

<td style="text-align:left;">

2394.0 (51.5)

</td>

<td style="text-align:left;">

1494.3 (58.0)

</td>

<td style="text-align:left;">

404.1 (60.8)

</td>

<td style="text-align:left;">

344.3 (71.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

never married

</td>

<td style="text-align:left;">

1433.5 (30.8)

</td>

<td style="text-align:left;">

649.9 (25.2)

</td>

<td style="text-align:left;">

118.4 (17.8)

</td>

<td style="text-align:left;">

49.3 (10.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

wid/div/sep

</td>

<td style="text-align:left;">

823.6 (17.7)

</td>

<td style="text-align:left;">

431.3 (16.7)

</td>

<td style="text-align:left;">

141.7 (21.3)

</td>

<td style="text-align:left;">

85.2 (17.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

region (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Midwest

</td>

<td style="text-align:left;">

1095.8 (23.6)

</td>

<td style="text-align:left;">

568.7 (22.1)

</td>

<td style="text-align:left;">

127.0 (19.1)

</td>

<td style="text-align:left;">

102.4 (21.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

Northeast

</td>

<td style="text-align:left;">

761.6 (16.4)

</td>

<td style="text-align:left;">

470.4 (18.3)

</td>

<td style="text-align:left;">

100.4 (15.1)

</td>

<td style="text-align:left;">

63.3 (13.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

South

</td>

<td style="text-align:left;">

1831.2 (39.4)

</td>

<td style="text-align:left;">

923.7 (35.9)

</td>

<td style="text-align:left;">

262.0 (39.4)

</td>

<td style="text-align:left;">

162.1 (33.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

West

</td>

<td style="text-align:left;">

962.8 (20.7)

</td>

<td style="text-align:left;">

612.7 (23.8)

</td>

<td style="text-align:left;">

174.9 (26.3)

</td>

<td style="text-align:left;">

151.0 (31.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

income (median \[IQR\])

</td>

<td style="text-align:left;">

63838.02 \[35022.03, 102218.99\]

</td>

<td style="text-align:left;">

84330.67 \[48492.05, 128628.24\]

</td>

<td style="text-align:left;">

63838.02 \[33691.44, 122662.79\]

</td>

<td style="text-align:left;">

128628.24 \[70745.46, 233597.37\]

</td>

</tr>

<tr>

<td style="text-align:left;">

age (median \[IQR\])

</td>

<td style="text-align:left;">

40.00 \[29.00, 51.00\]

</td>

<td style="text-align:left;">

42.00 \[32.00, 51.00\]

</td>

<td style="text-align:left;">

49.00 \[37.00, 58.00\]

</td>

<td style="text-align:left;">

50.00 \[40.00, 57.00\]

</td>

</tr>

<tr>

<td style="text-align:left;">

srh\_bin = poor/fair (%)

</td>

<td style="text-align:left;">

579.3 (15.2)

</td>

<td style="text-align:left;">

271.5 (12.7)

</td>

<td style="text-align:left;">

83.4 (15.7)

</td>

<td style="text-align:left;">

42.9 (11.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

mntlhlth (mean (SD))

</td>

<td style="text-align:left;">

3.55 (7.14)

</td>

<td style="text-align:left;">

3.34 (6.83)

</td>

<td style="text-align:left;">

2.57 (6.15)

</td>

<td style="text-align:left;">

2.96 (7.04)

</td>

</tr>

<tr>

<td style="text-align:left;">

depress = no (%)

</td>

<td style="text-align:left;">

1285.7 (82.6)

</td>

<td style="text-align:left;">

690.7 (85.3)

</td>

<td style="text-align:left;">

174.6 (82.0)

</td>

<td style="text-align:left;">

128.4 (90.5)

</td>

</tr>

</tbody>

</table>

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<caption>

Complex

</caption>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

Less-skilled workers

</th>

<th style="text-align:left;">

More-skilled workers

</th>

<th style="text-align:left;">

Low-level managers

</th>

<th style="text-align:left;">

High-level managers

</th>

<th style="text-align:left;">

Petit bourgeoisie

</th>

<th style="text-align:left;">

Small capitalists

</th>

<th style="text-align:left;">

Large capitalists

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

n

</td>

<td style="text-align:left;">

2501.6

</td>

<td style="text-align:left;">

2144.4

</td>

<td style="text-align:left;">

1869.4

</td>

<td style="text-align:left;">

699.7

</td>

<td style="text-align:left;">

664.2

</td>

<td style="text-align:left;">

271.6

</td>

<td style="text-align:left;">

205.8

</td>

</tr>

<tr>

<td style="text-align:left;">

sex = female (%)

</td>

<td style="text-align:left;">

1213.3 (48.5)

</td>

<td style="text-align:left;">

1342.4 (62.6)

</td>

<td style="text-align:left;">

923.5 (49.4)

</td>

<td style="text-align:left;">

266.5 (38.1)

</td>

<td style="text-align:left;">

327.3 (49.3)

</td>

<td style="text-align:left;">

73.3 (27.0)

</td>

<td style="text-align:left;">

47.6 (23.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

race (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

white

</td>

<td style="text-align:left;">

1637.6 (65.5)

</td>

<td style="text-align:left;">

1651.4 (77.0)

</td>

<td style="text-align:left;">

1429.9 (76.5)

</td>

<td style="text-align:left;">

538.0 (76.9)

</td>

<td style="text-align:left;">

527.6 (79.4)

</td>

<td style="text-align:left;">

218.3 (80.4)

</td>

<td style="text-align:left;">

182.2 (88.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

black

</td>

<td style="text-align:left;">

452.7 (18.1)

</td>

<td style="text-align:left;">

273.3 (12.7)

</td>

<td style="text-align:left;">

227.5 (12.2)

</td>

<td style="text-align:left;">

79.1 (11.3)

</td>

<td style="text-align:left;">

46.8 (7.0)

</td>

<td style="text-align:left;">

19.3 (7.1)

</td>

<td style="text-align:left;">

7.1 (3.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

other

</td>

<td style="text-align:left;">

411.3 (16.4)

</td>

<td style="text-align:left;">

219.7 (10.2)

</td>

<td style="text-align:left;">

211.9 (11.3)

</td>

<td style="text-align:left;">

82.5 (11.8)

</td>

<td style="text-align:left;">

89.8 (13.5)

</td>

<td style="text-align:left;">

34.0 (12.5)

</td>

<td style="text-align:left;">

16.4 (8.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

educ (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

\<HS

</td>

<td style="text-align:left;">

431.1 (17.2)

</td>

<td style="text-align:left;">

86.2 (4.0)

</td>

<td style="text-align:left;">

160.7 (8.6)

</td>

<td style="text-align:left;">

34.3 (4.9)

</td>

<td style="text-align:left;">

87.0 (13.1)

</td>

<td style="text-align:left;">

27.9 (10.3)

</td>

<td style="text-align:left;">

8.4 (4.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

HS

</td>

<td style="text-align:left;">

1652.5 (66.1)

</td>

<td style="text-align:left;">

876.1 (40.9)

</td>

<td style="text-align:left;">

880.6 (47.1)

</td>

<td style="text-align:left;">

273.5 (39.1)

</td>

<td style="text-align:left;">

317.8 (47.9)

</td>

<td style="text-align:left;">

117.4 (43.2)

</td>

<td style="text-align:left;">

77.3 (37.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

JuCo

</td>

<td style="text-align:left;">

155.8 (6.2)

</td>

<td style="text-align:left;">

248.6 (11.6)

</td>

<td style="text-align:left;">

174.3 (9.3)

</td>

<td style="text-align:left;">

81.6 (11.7)

</td>

<td style="text-align:left;">

56.5 (8.5)

</td>

<td style="text-align:left;">

25.5 (9.4)

</td>

<td style="text-align:left;">

11.4 (5.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

College +

</td>

<td style="text-align:left;">

262.2 (10.5)

</td>

<td style="text-align:left;">

933.6 (43.5)

</td>

<td style="text-align:left;">

653.8 (35.0)

</td>

<td style="text-align:left;">

310.3 (44.4)

</td>

<td style="text-align:left;">

202.9 (30.5)

</td>

<td style="text-align:left;">

100.8 (37.1)

</td>

<td style="text-align:left;">

108.7 (52.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

marital\_tri (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

married

</td>

<td style="text-align:left;">

1143.5 (45.7)

</td>

<td style="text-align:left;">

1249.6 (58.3)

</td>

<td style="text-align:left;">

1025.7 (54.9)

</td>

<td style="text-align:left;">

463.8 (66.3)

</td>

<td style="text-align:left;">

404.1 (60.8)

</td>

<td style="text-align:left;">

191.3 (70.4)

</td>

<td style="text-align:left;">

151.6 (73.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

never married

</td>

<td style="text-align:left;">

892.8 (35.7)

</td>

<td style="text-align:left;">

536.2 (25.0)

</td>

<td style="text-align:left;">

528.7 (28.3)

</td>

<td style="text-align:left;">

120.1 (17.2)

</td>

<td style="text-align:left;">

118.4 (17.8)

</td>

<td style="text-align:left;">

33.5 (12.3)

</td>

<td style="text-align:left;">

15.8 (7.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

wid/div/sep

</td>

<td style="text-align:left;">

464.9 (18.6)

</td>

<td style="text-align:left;">

358.6 (16.7)

</td>

<td style="text-align:left;">

315.0 (16.8)

</td>

<td style="text-align:left;">

115.8 (16.5)

</td>

<td style="text-align:left;">

141.7 (21.3)

</td>

<td style="text-align:left;">

46.8 (17.2)

</td>

<td style="text-align:left;">

38.4 (18.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

region (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Midwest

</td>

<td style="text-align:left;">

617.1 (24.7)

</td>

<td style="text-align:left;">

474.2 (22.1)

</td>

<td style="text-align:left;">

418.3 (22.4)

</td>

<td style="text-align:left;">

149.2 (21.3)

</td>

<td style="text-align:left;">

127.0 (19.1)

</td>

<td style="text-align:left;">

59.5 (21.9)

</td>

<td style="text-align:left;">

42.9 (20.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

Northeast

</td>

<td style="text-align:left;">

367.6 (14.7)

</td>

<td style="text-align:left;">

393.9 (18.4)

</td>

<td style="text-align:left;">

350.6 (18.8)

</td>

<td style="text-align:left;">

118.9 (17.0)

</td>

<td style="text-align:left;">

100.4 (15.1)

</td>

<td style="text-align:left;">

39.3 (14.5)

</td>

<td style="text-align:left;">

24.0 (11.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

South

</td>

<td style="text-align:left;">

1019.7 (40.8)

</td>

<td style="text-align:left;">

811.5 (37.8)

</td>

<td style="text-align:left;">

669.0 (35.8)

</td>

<td style="text-align:left;">

250.3 (35.8)

</td>

<td style="text-align:left;">

262.0 (39.4)

</td>

<td style="text-align:left;">

90.5 (33.3)

</td>

<td style="text-align:left;">

70.1 (34.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

West

</td>

<td style="text-align:left;">

497.2 (19.9)

</td>

<td style="text-align:left;">

464.7 (21.7)

</td>

<td style="text-align:left;">

431.4 (23.1)

</td>

<td style="text-align:left;">

181.3 (25.9)

</td>

<td style="text-align:left;">

174.9 (26.3)

</td>

<td style="text-align:left;">

82.3 (30.3)

</td>

<td style="text-align:left;">

68.7 (33.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

income (median \[IQR\])

</td>

<td style="text-align:left;">

48492.05 \[27333.74, 84330.67\]

</td>

<td style="text-align:left;">

78023.27 \[48235.41, 117077.93\]

</td>

<td style="text-align:left;">

78023.27 \[46104.99, 122662.79\]

</td>

<td style="text-align:left;">

96589.29 \[57882.78, 154352.13\]

</td>

<td style="text-align:left;">

63838.02 \[33691.44, 122662.79\]

</td>

<td style="text-align:left;">

88902.09 \[58486.69, 222968.91\]

</td>

<td style="text-align:left;">

222968.91 \[96589.29, 234392.40\]

</td>

</tr>

<tr>

<td style="text-align:left;">

age (median \[IQR\])

</td>

<td style="text-align:left;">

39.00 \[27.00, 50.00\]

</td>

<td style="text-align:left;">

41.00 \[31.00, 52.00\]

</td>

<td style="text-align:left;">

40.00 \[30.00, 51.00\]

</td>

<td style="text-align:left;">

45.00 \[35.00, 53.00\]

</td>

<td style="text-align:left;">

49.00 \[37.00, 58.00\]

</td>

<td style="text-align:left;">

48.00 \[38.00, 58.00\]

</td>

<td style="text-align:left;">

52.00 \[44.00, 57.00\]

</td>

</tr>

<tr>

<td style="text-align:left;">

srh\_bin = poor/fair (%)

</td>

<td style="text-align:left;">

384.7 (19.1)

</td>

<td style="text-align:left;">

194.6 (10.9)

</td>

<td style="text-align:left;">

204.8 (13.1)

</td>

<td style="text-align:left;">

66.7 (11.7)

</td>

<td style="text-align:left;">

83.4 (15.7)

</td>

<td style="text-align:left;">

17.4 (7.6)

</td>

<td style="text-align:left;">

25.5 (15.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

mntlhlth (mean (SD))

</td>

<td style="text-align:left;">

3.85 (7.62)

</td>

<td style="text-align:left;">

3.19 (6.54)

</td>

<td style="text-align:left;">

3.51 (7.03)

</td>

<td style="text-align:left;">

2.90 (6.25)

</td>

<td style="text-align:left;">

2.57 (6.15)

</td>

<td style="text-align:left;">

3.06 (7.30)

</td>

<td style="text-align:left;">

2.83 (6.69)

</td>

</tr>

<tr>

<td style="text-align:left;">

depress = no (%)

</td>

<td style="text-align:left;">

692.6 (83.8)

</td>

<td style="text-align:left;">

592.2 (81.8)

</td>

<td style="text-align:left;">

481.6 (84.8)

</td>

<td style="text-align:left;">

209.0 (86.6)

</td>

<td style="text-align:left;">

174.6 (82.0)

</td>

<td style="text-align:left;">

69.5 (91.4)

</td>

<td style="text-align:left;">

57.5 (89.1)

</td>

</tr>

</tbody>

</table>

## QWL variables stratified by class (weighted but unimputed)

  - The QWL variables analyzed are:
      - mustwork: Mandatory to work extra hours
      - chngtme: How often r allowed change schedule
      - famwkoff: How hard to take time off
      - wkvsfam: How often job interferes fam life
      - secondwk: R has job other than main
      - learnnew: Job requires r to learn new things
      - workfast: Job requires r to work fast
      - wrktime: R has enough time to get the job done
      - workdiff: R does numerous things on job
      - toofewwk: How often not enough staff
      - overwork: R has too much work to do well
      - myskills: Job allows r use of skills
      - trainops: R have the training opportunities
      - opdevel: Opportunity to develop my abilities
      - respect: R treated with respect at work
      - trustman: R trust management at work
      - manvsemp: Relations bw management and employees
      - suphelp: Supervisor helpful to r in getting job done
      - supcares: Supervisor concerned about welfare
      - wkfreedm: A lot of freedom to decide how to do job
      - lotofsay: R has lot of say in job
      - wkdecide: How often r take part in decisions
      - satjob1: Job satisfaction in general
      - fairearn: How fair is what r earn on the job
      - fringeok: Fringe benefits are good
      - rincblls: Income alone is enough
      - laidoff: R was laid off main job last year
      - jobsecok: The job security is good
      - any\_discrim\_harass: Any discrimination or harassment on job
        (created by me from several more specific variables focused
        separately on racism, sexism, etc.)
      - safetywk: Worker safety priority at work
      - safehlth: Safety and health condition good at work
      - safefrst: No shortcuts on worker safety

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<caption>

Simple

</caption>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

Workers

</th>

<th style="text-align:left;">

Managers

</th>

<th style="text-align:left;">

Petit bourgeoisie

</th>

<th style="text-align:left;">

Capitalists

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

n

</td>

<td style="text-align:left;">

4651.5

</td>

<td style="text-align:left;">

2575.5

</td>

<td style="text-align:left;">

664.2

</td>

<td style="text-align:left;">

478.8

</td>

</tr>

<tr>

<td style="text-align:left;">

mustwork = no (%)

</td>

<td style="text-align:left;">

2832.6 (75.4)

</td>

<td style="text-align:left;">

1484.4 (69.9)

</td>

<td style="text-align:left;">

395.6 (76.2)

</td>

<td style="text-align:left;">

245.2 (65.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

chngtme (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

often

</td>

<td style="text-align:left;">

662.7 (22.4)

</td>

<td style="text-align:left;">

595.8 (35.2)

</td>

<td style="text-align:left;">

316.7 (75.7)

</td>

<td style="text-align:left;">

228.1 (72.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

sometimes

</td>

<td style="text-align:left;">

621.1 (21.0)

</td>

<td style="text-align:left;">

383.9 (22.7)

</td>

<td style="text-align:left;">

48.1 (11.5)

</td>

<td style="text-align:left;">

42.0 (13.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

rarely

</td>

<td style="text-align:left;">

523.6 (17.7)

</td>

<td style="text-align:left;">

270.1 (16.0)

</td>

<td style="text-align:left;">

20.7 (4.9)

</td>

<td style="text-align:left;">

16.1 (5.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

never

</td>

<td style="text-align:left;">

1149.5 (38.9)

</td>

<td style="text-align:left;">

441.3 (26.1)

</td>

<td style="text-align:left;">

32.8 (7.8)

</td>

<td style="text-align:left;">

28.7 (9.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

famwkoff (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all hard

</td>

<td style="text-align:left;">

1573.9 (41.5)

</td>

<td style="text-align:left;">

853.1 (40.0)

</td>

<td style="text-align:left;">

301.6 (57.0)

</td>

<td style="text-align:left;">

163.2 (42.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too hard

</td>

<td style="text-align:left;">

1215.1 (32.0)

</td>

<td style="text-align:left;">

683.6 (32.1)

</td>

<td style="text-align:left;">

113.4 (21.4)

</td>

<td style="text-align:left;">

101.9 (26.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat hard

</td>

<td style="text-align:left;">

643.0 (16.9)

</td>

<td style="text-align:left;">

376.7 (17.7)

</td>

<td style="text-align:left;">

74.5 (14.1)

</td>

<td style="text-align:left;">

69.0 (17.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

very hard

</td>

<td style="text-align:left;">

364.0 (9.6)

</td>

<td style="text-align:left;">

217.1 (10.2)

</td>

<td style="text-align:left;">

40.0 (7.6)

</td>

<td style="text-align:left;">

52.5 (13.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

wkvsfam (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

often

</td>

<td style="text-align:left;">

372.7 (9.8)

</td>

<td style="text-align:left;">

324.3 (15.2)

</td>

<td style="text-align:left;">

57.4 (10.8)

</td>

<td style="text-align:left;">

100.0 (25.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

sometimes

</td>

<td style="text-align:left;">

1022.2 (26.8)

</td>

<td style="text-align:left;">

776.6 (36.4)

</td>

<td style="text-align:left;">

159.6 (30.1)

</td>

<td style="text-align:left;">

133.4 (34.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

rarely

</td>

<td style="text-align:left;">

1251.6 (32.9)

</td>

<td style="text-align:left;">

641.7 (30.1)

</td>

<td style="text-align:left;">

167.4 (31.6)

</td>

<td style="text-align:left;">

101.5 (26.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

never

</td>

<td style="text-align:left;">

1161.7 (30.5)

</td>

<td style="text-align:left;">

389.3 (18.3)

</td>

<td style="text-align:left;">

145.6 (27.5)

</td>

<td style="text-align:left;">

52.6 (13.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

secondwk = no (%)

</td>

<td style="text-align:left;">

3193.9 (83.9)

</td>

<td style="text-align:left;">

1761.6 (82.6)

</td>

<td style="text-align:left;">

418.2 (78.7)

</td>

<td style="text-align:left;">

326.5 (83.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

learnnew (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

1455.2 (38.3)

</td>

<td style="text-align:left;">

1095.9 (51.4)

</td>

<td style="text-align:left;">

237.8 (44.8)

</td>

<td style="text-align:left;">

203.5 (52.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

1697.9 (44.6)

</td>

<td style="text-align:left;">

866.9 (40.7)

</td>

<td style="text-align:left;">

224.4 (42.3)

</td>

<td style="text-align:left;">

166.7 (43.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

510.9 (13.4)

</td>

<td style="text-align:left;">

152.8 (7.2)

</td>

<td style="text-align:left;">

53.6 (10.1)

</td>

<td style="text-align:left;">

12.7 (3.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

139.1 (3.7)

</td>

<td style="text-align:left;">

16.3 (0.8)

</td>

<td style="text-align:left;">

15.1 (2.8)

</td>

<td style="text-align:left;">

3.8 (1.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

workfast (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

774.9 (20.4)

</td>

<td style="text-align:left;">

560.8 (26.3)

</td>

<td style="text-align:left;">

78.5 (14.8)

</td>

<td style="text-align:left;">

101.8 (26.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

1677.0 (44.1)

</td>

<td style="text-align:left;">

1023.2 (48.0)

</td>

<td style="text-align:left;">

196.2 (37.0)

</td>

<td style="text-align:left;">

179.4 (46.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

1201.3 (31.6)

</td>

<td style="text-align:left;">

519.9 (24.4)

</td>

<td style="text-align:left;">

217.0 (40.9)

</td>

<td style="text-align:left;">

94.6 (24.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

145.7 (3.8)

</td>

<td style="text-align:left;">

26.8 (1.3)

</td>

<td style="text-align:left;">

38.3 (7.2)

</td>

<td style="text-align:left;">

10.9 (2.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

wrktime (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

1721.4 (45.3)

</td>

<td style="text-align:left;">

715.5 (33.5)

</td>

<td style="text-align:left;">

330.2 (63.8)

</td>

<td style="text-align:left;">

184.6 (49.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

1543.2 (40.6)

</td>

<td style="text-align:left;">

944.6 (44.3)

</td>

<td style="text-align:left;">

149.3 (28.9)

</td>

<td style="text-align:left;">

146.5 (39.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

384.8 (10.1)

</td>

<td style="text-align:left;">

323.2 (15.1)

</td>

<td style="text-align:left;">

29.2 (5.7)

</td>

<td style="text-align:left;">

33.0 (8.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

153.3 (4.0)

</td>

<td style="text-align:left;">

150.4 (7.0)

</td>

<td style="text-align:left;">

8.7 (1.7)

</td>

<td style="text-align:left;">

10.1 (2.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

workdiff (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

1246.7 (32.8)

</td>

<td style="text-align:left;">

1053.1 (49.3)

</td>

<td style="text-align:left;">

203.5 (38.5)

</td>

<td style="text-align:left;">

200.8 (51.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

2023.6 (53.3)

</td>

<td style="text-align:left;">

966.8 (45.3)

</td>

<td style="text-align:left;">

263.4 (49.8)

</td>

<td style="text-align:left;">

165.0 (42.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

460.0 (12.1)

</td>

<td style="text-align:left;">

107.3 (5.0)

</td>

<td style="text-align:left;">

52.2 (9.9)

</td>

<td style="text-align:left;">

17.5 (4.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

69.4 (1.8)

</td>

<td style="text-align:left;">

6.9 (0.3)

</td>

<td style="text-align:left;">

10.0 (1.9)

</td>

<td style="text-align:left;">

3.4 (0.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

toofewwk (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

often

</td>

<td style="text-align:left;">

941.6 (24.8)

</td>

<td style="text-align:left;">

644.4 (30.2)

</td>

<td style="text-align:left;">

67.9 (13.1)

</td>

<td style="text-align:left;">

79.2 (20.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

sometimes

</td>

<td style="text-align:left;">

1248.5 (32.9)

</td>

<td style="text-align:left;">

825.6 (38.7)

</td>

<td style="text-align:left;">

109.2 (21.1)

</td>

<td style="text-align:left;">

150.1 (39.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

rarely

</td>

<td style="text-align:left;">

1129.9 (29.8)

</td>

<td style="text-align:left;">

505.6 (23.7)

</td>

<td style="text-align:left;">

136.5 (26.4)

</td>

<td style="text-align:left;">

108.2 (28.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

never

</td>

<td style="text-align:left;">

474.8 (12.5)

</td>

<td style="text-align:left;">

156.4 (7.3)

</td>

<td style="text-align:left;">

203.6 (39.4)

</td>

<td style="text-align:left;">

44.9 (11.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

overwork (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

247.2 (6.5)

</td>

<td style="text-align:left;">

199.9 (9.4)

</td>

<td style="text-align:left;">

17.2 (3.3)

</td>

<td style="text-align:left;">

35.5 (9.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

840.6 (22.1)

</td>

<td style="text-align:left;">

581.8 (27.3)

</td>

<td style="text-align:left;">

92.6 (17.5)

</td>

<td style="text-align:left;">

97.6 (25.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

2303.4 (60.7)

</td>

<td style="text-align:left;">

1176.3 (55.2)

</td>

<td style="text-align:left;">

348.0 (65.8)

</td>

<td style="text-align:left;">

218.3 (56.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

406.3 (10.7)

</td>

<td style="text-align:left;">

171.8 (8.1)

</td>

<td style="text-align:left;">

70.9 (13.4)

</td>

<td style="text-align:left;">

33.9 (8.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

myskills (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

1215.1 (32.0)

</td>

<td style="text-align:left;">

989.9 (46.4)

</td>

<td style="text-align:left;">

262.9 (49.6)

</td>

<td style="text-align:left;">

204.5 (52.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

2130.4 (56.0)

</td>

<td style="text-align:left;">

1016.7 (47.7)

</td>

<td style="text-align:left;">

236.1 (44.5)

</td>

<td style="text-align:left;">

165.0 (42.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

372.5 (9.8)

</td>

<td style="text-align:left;">

111.8 (5.2)

</td>

<td style="text-align:left;">

24.0 (4.5)

</td>

<td style="text-align:left;">

15.0 (3.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

85.2 (2.2)

</td>

<td style="text-align:left;">

14.9 (0.7)

</td>

<td style="text-align:left;">

7.1 (1.3)

</td>

<td style="text-align:left;">

2.2 (0.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

trainops (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

947.5 (60.2)

</td>

<td style="text-align:left;">

602.6 (61.9)

</td>

<td style="text-align:left;">

149.1 (72.1)

</td>

<td style="text-align:left;">

122.9 (79.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

498.0 (31.6)

</td>

<td style="text-align:left;">

308.6 (31.7)

</td>

<td style="text-align:left;">

38.8 (18.8)

</td>

<td style="text-align:left;">

28.4 (18.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

80.7 (5.1)

</td>

<td style="text-align:left;">

41.6 (4.3)

</td>

<td style="text-align:left;">

7.1 (3.5)

</td>

<td style="text-align:left;">

2.8 (1.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

49.0 (3.1)

</td>

<td style="text-align:left;">

20.5 (2.1)

</td>

<td style="text-align:left;">

11.8 (5.7)

</td>

<td style="text-align:left;">

0.9 (0.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

opdevel (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

1160.6 (30.6)

</td>

<td style="text-align:left;">

923.7 (43.4)

</td>

<td style="text-align:left;">

332.1 (64.1)

</td>

<td style="text-align:left;">

259.5 (69.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

1694.6 (44.7)

</td>

<td style="text-align:left;">

933.7 (43.9)

</td>

<td style="text-align:left;">

129.0 (24.9)

</td>

<td style="text-align:left;">

92.2 (24.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

639.1 (16.8)

</td>

<td style="text-align:left;">

192.4 (9.0)

</td>

<td style="text-align:left;">

32.4 (6.3)

</td>

<td style="text-align:left;">

15.6 (4.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

299.5 (7.9)

</td>

<td style="text-align:left;">

76.9 (3.6)

</td>

<td style="text-align:left;">

24.4 (4.7)

</td>

<td style="text-align:left;">

8.7 (2.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

respect (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

1292.0 (34.0)

</td>

<td style="text-align:left;">

891.5 (41.8)

</td>

<td style="text-align:left;">

244.5 (46.6)

</td>

<td style="text-align:left;">

187.2 (48.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

2173.1 (57.2)

</td>

<td style="text-align:left;">

1106.8 (51.9)

</td>

<td style="text-align:left;">

258.2 (49.2)

</td>

<td style="text-align:left;">

186.6 (48.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

253.0 (6.7)

</td>

<td style="text-align:left;">

110.1 (5.2)

</td>

<td style="text-align:left;">

20.4 (3.9)

</td>

<td style="text-align:left;">

7.0 (1.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

82.0 (2.2)

</td>

<td style="text-align:left;">

24.0 (1.1)

</td>

<td style="text-align:left;">

2.0 (0.4)

</td>

<td style="text-align:left;">

1.7 (0.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

trustman (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

906.0 (23.9)

</td>

<td style="text-align:left;">

654.8 (30.9)

</td>

<td style="text-align:left;">

275.5 (53.9)

</td>

<td style="text-align:left;">

213.5 (57.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

2051.3 (54.1)

</td>

<td style="text-align:left;">

1066.4 (50.3)

</td>

<td style="text-align:left;">

209.2 (40.9)

</td>

<td style="text-align:left;">

141.4 (38.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

626.8 (16.5)

</td>

<td style="text-align:left;">

313.2 (14.8)

</td>

<td style="text-align:left;">

23.3 (4.6)

</td>

<td style="text-align:left;">

13.0 (3.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

207.1 (5.5)

</td>

<td style="text-align:left;">

87.6 (4.1)

</td>

<td style="text-align:left;">

3.2 (0.6)

</td>

<td style="text-align:left;">

3.1 (0.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

manvsemp (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very good

</td>

<td style="text-align:left;">

1191.8 (31.4)

</td>

<td style="text-align:left;">

691.6 (32.5)

</td>

<td style="text-align:left;">

270.5 (61.2)

</td>

<td style="text-align:left;">

229.9 (60.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

quite good

</td>

<td style="text-align:left;">

1408.2 (37.1)

</td>

<td style="text-align:left;">

871.8 (40.9)

</td>

<td style="text-align:left;">

91.2 (20.6)

</td>

<td style="text-align:left;">

114.8 (30.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

neither good nor bad

</td>

<td style="text-align:left;">

872.5 (23.0)

</td>

<td style="text-align:left;">

425.4 (20.0)

</td>

<td style="text-align:left;">

70.4 (15.9)

</td>

<td style="text-align:left;">

27.8 (7.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

quite bad

</td>

<td style="text-align:left;">

242.1 (6.4)

</td>

<td style="text-align:left;">

110.6 (5.2)

</td>

<td style="text-align:left;">

7.6 (1.7)

</td>

<td style="text-align:left;">

3.7 (1.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

very bad

</td>

<td style="text-align:left;">

76.5 (2.0)

</td>

<td style="text-align:left;">

30.8 (1.4)

</td>

<td style="text-align:left;">

2.7 (0.6)

</td>

<td style="text-align:left;">

1.1 (0.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

suphelp (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

1807.5 (47.7)

</td>

<td style="text-align:left;">

1049.2 (49.5)

</td>

<td style="text-align:left;">

253.3 (59.5)

</td>

<td style="text-align:left;">

202.0 (66.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

1431.4 (37.8)

</td>

<td style="text-align:left;">

764.0 (36.1)

</td>

<td style="text-align:left;">

94.6 (22.2)

</td>

<td style="text-align:left;">

69.7 (22.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

338.5 (8.9)

</td>

<td style="text-align:left;">

192.4 (9.1)

</td>

<td style="text-align:left;">

25.3 (5.9)

</td>

<td style="text-align:left;">

20.4 (6.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

209.7 (5.5)

</td>

<td style="text-align:left;">

113.0 (5.3)

</td>

<td style="text-align:left;">

52.3 (12.3)

</td>

<td style="text-align:left;">

13.5 (4.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

supcares (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

1794.9 (47.6)

</td>

<td style="text-align:left;">

1129.7 (53.3)

</td>

<td style="text-align:left;">

234.4 (56.1)

</td>

<td style="text-align:left;">

215.5 (68.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

1367.9 (36.3)

</td>

<td style="text-align:left;">

688.0 (32.5)

</td>

<td style="text-align:left;">

92.8 (22.2)

</td>

<td style="text-align:left;">

57.8 (18.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

367.6 (9.7)

</td>

<td style="text-align:left;">

188.7 (8.9)

</td>

<td style="text-align:left;">

25.0 (6.0)

</td>

<td style="text-align:left;">

16.9 (5.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

242.1 (6.4)

</td>

<td style="text-align:left;">

112.9 (5.3)

</td>

<td style="text-align:left;">

65.7 (15.7)

</td>

<td style="text-align:left;">

23.4 (7.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

wkfreedm (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

1805.3 (47.5)

</td>

<td style="text-align:left;">

1247.1 (58.5)

</td>

<td style="text-align:left;">

447.9 (86.4)

</td>

<td style="text-align:left;">

317.6 (84.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

1383.9 (36.4)

</td>

<td style="text-align:left;">

682.3 (32.0)

</td>

<td style="text-align:left;">

50.9 (9.8)

</td>

<td style="text-align:left;">

54.6 (14.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

426.2 (11.2)

</td>

<td style="text-align:left;">

152.6 (7.2)

</td>

<td style="text-align:left;">

10.5 (2.0)

</td>

<td style="text-align:left;">

4.3 (1.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

187.6 (4.9)

</td>

<td style="text-align:left;">

49.9 (2.3)

</td>

<td style="text-align:left;">

9.1 (1.7)

</td>

<td style="text-align:left;">

0.9 (0.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

lotofsay (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

266.0 (16.9)

</td>

<td style="text-align:left;">

383.6 (39.3)

</td>

<td style="text-align:left;">

131.5 (58.4)

</td>

<td style="text-align:left;">

118.2 (70.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

663.5 (42.1)

</td>

<td style="text-align:left;">

435.8 (44.6)

</td>

<td style="text-align:left;">

74.8 (33.2)

</td>

<td style="text-align:left;">

44.7 (26.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

497.8 (31.6)

</td>

<td style="text-align:left;">

132.7 (13.6)

</td>

<td style="text-align:left;">

16.7 (7.4)

</td>

<td style="text-align:left;">

4.4 (2.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

147.9 (9.4)

</td>

<td style="text-align:left;">

24.6 (2.5)

</td>

<td style="text-align:left;">

2.3 (1.0)

</td>

<td style="text-align:left;">

0.0 (0.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

wkdecide (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

often

</td>

<td style="text-align:left;">

1146.4 (30.1)

</td>

<td style="text-align:left;">

1146.8 (53.8)

</td>

<td style="text-align:left;">

181.9 (34.7)

</td>

<td style="text-align:left;">

217.4 (57.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

sometimes

</td>

<td style="text-align:left;">

1576.0 (41.4)

</td>

<td style="text-align:left;">

754.5 (35.4)

</td>

<td style="text-align:left;">

130.5 (24.9)

</td>

<td style="text-align:left;">

99.8 (26.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

rarely

</td>

<td style="text-align:left;">

696.9 (18.3)

</td>

<td style="text-align:left;">

163.1 (7.6)

</td>

<td style="text-align:left;">

85.2 (16.3)

</td>

<td style="text-align:left;">

41.3 (10.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

never

</td>

<td style="text-align:left;">

384.7 (10.1)

</td>

<td style="text-align:left;">

68.4 (3.2)

</td>

<td style="text-align:left;">

126.4 (24.1)

</td>

<td style="text-align:left;">

21.0 (5.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

satjob1 (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very satisfied

</td>

<td style="text-align:left;">

1677.5 (44.1)

</td>

<td style="text-align:left;">

1042.2 (48.9)

</td>

<td style="text-align:left;">

329.3 (62.4)

</td>

<td style="text-align:left;">

253.7 (65.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat satisfied

</td>

<td style="text-align:left;">

1660.7 (43.7)

</td>

<td style="text-align:left;">

902.8 (42.4)

</td>

<td style="text-align:left;">

167.9 (31.8)

</td>

<td style="text-align:left;">

118.8 (30.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too satisfied

</td>

<td style="text-align:left;">

315.7 (8.3)

</td>

<td style="text-align:left;">

151.5 (7.1)

</td>

<td style="text-align:left;">

19.6 (3.7)

</td>

<td style="text-align:left;">

8.2 (2.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all satisfied

</td>

<td style="text-align:left;">

146.9 (3.9)

</td>

<td style="text-align:left;">

34.1 (1.6)

</td>

<td style="text-align:left;">

10.8 (2.1)

</td>

<td style="text-align:left;">

9.6 (2.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

fairearn (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

much less than you deserve

</td>

<td style="text-align:left;">

491.0 (13.1)

</td>

<td style="text-align:left;">

260.7 (12.4)

</td>

<td style="text-align:left;">

44.3 (9.0)

</td>

<td style="text-align:left;">

36.4 (10.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat less than you deserve

</td>

<td style="text-align:left;">

1086.7 (29.0)

</td>

<td style="text-align:left;">

678.8 (32.2)

</td>

<td style="text-align:left;">

81.0 (16.4)

</td>

<td style="text-align:left;">

64.1 (17.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

about as much as you deserve

</td>

<td style="text-align:left;">

1865.0 (49.9)

</td>

<td style="text-align:left;">

979.3 (46.5)

</td>

<td style="text-align:left;">

309.6 (62.6)

</td>

<td style="text-align:left;">

223.1 (61.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat more than you deserve

</td>

<td style="text-align:left;">

239.0 (6.4)

</td>

<td style="text-align:left;">

155.5 (7.4)

</td>

<td style="text-align:left;">

47.8 (9.7)

</td>

<td style="text-align:left;">

24.5 (6.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

much more than you deserve

</td>

<td style="text-align:left;">

59.2 (1.6)

</td>

<td style="text-align:left;">

33.9 (1.6)

</td>

<td style="text-align:left;">

11.5 (2.3)

</td>

<td style="text-align:left;">

14.6 (4.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

fringeok (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

1315.5 (35.0)

</td>

<td style="text-align:left;">

852.3 (40.2)

</td>

<td style="text-align:left;">

178.9 (35.3)

</td>

<td style="text-align:left;">

184.0 (49.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

1355.0 (36.0)

</td>

<td style="text-align:left;">

774.7 (36.5)

</td>

<td style="text-align:left;">

117.8 (23.2)

</td>

<td style="text-align:left;">

91.1 (24.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

461.0 (12.3)

</td>

<td style="text-align:left;">

248.8 (11.7)

</td>

<td style="text-align:left;">

57.8 (11.4)

</td>

<td style="text-align:left;">

40.2 (10.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

628.0 (16.7)

</td>

<td style="text-align:left;">

245.9 (11.6)

</td>

<td style="text-align:left;">

152.7 (30.1)

</td>

<td style="text-align:left;">

55.0 (14.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

rincblls = no (%)

</td>

<td style="text-align:left;">

2292.0 (60.5)

</td>

<td style="text-align:left;">

1021.4 (48.0)

</td>

<td style="text-align:left;">

287.3 (55.0)

</td>

<td style="text-align:left;">

111.8 (29.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

laidoff = no (%)

</td>

<td style="text-align:left;">

3543.2 (93.1)

</td>

<td style="text-align:left;">

2004.7 (93.9)

</td>

<td style="text-align:left;">

471.9 (89.9)

</td>

<td style="text-align:left;">

367.1 (95.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

jobsecok (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

1955.1 (51.7)

</td>

<td style="text-align:left;">

1247.5 (58.7)

</td>

<td style="text-align:left;">

292.2 (57.0)

</td>

<td style="text-align:left;">

244.9 (65.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

1302.7 (34.4)

</td>

<td style="text-align:left;">

649.4 (30.6)

</td>

<td style="text-align:left;">

128.4 (25.1)

</td>

<td style="text-align:left;">

88.2 (23.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

310.4 (8.2)

</td>

<td style="text-align:left;">

152.4 (7.2)

</td>

<td style="text-align:left;">

52.0 (10.1)

</td>

<td style="text-align:left;">

21.5 (5.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

215.2 (5.7)

</td>

<td style="text-align:left;">

75.9 (3.6)

</td>

<td style="text-align:left;">

39.9 (7.8)

</td>

<td style="text-align:left;">

17.2 (4.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

any\_discrim\_harass = yes (%)

</td>

<td style="text-align:left;">

789.4 (20.7)

</td>

<td style="text-align:left;">

504.2 (23.7)

</td>

<td style="text-align:left;">

72.6 (13.9)

</td>

<td style="text-align:left;">

63.1 (16.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

safetywk (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

1415.2 (37.5)

</td>

<td style="text-align:left;">

1030.8 (48.5)

</td>

<td style="text-align:left;">

251.0 (50.7)

</td>

<td style="text-align:left;">

233.3 (61.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

1997.7 (52.9)

</td>

<td style="text-align:left;">

930.3 (43.7)

</td>

<td style="text-align:left;">

227.0 (45.9)

</td>

<td style="text-align:left;">

129.9 (34.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

300.1 (7.9)

</td>

<td style="text-align:left;">

137.8 (6.5)

</td>

<td style="text-align:left;">

13.8 (2.8)

</td>

<td style="text-align:left;">

12.9 (3.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

62.5 (1.7)

</td>

<td style="text-align:left;">

27.7 (1.3)

</td>

<td style="text-align:left;">

3.2 (0.6)

</td>

<td style="text-align:left;">

2.2 (0.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

safehlth (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

1248.0 (32.9)

</td>

<td style="text-align:left;">

900.8 (42.3)

</td>

<td style="text-align:left;">

242.0 (47.3)

</td>

<td style="text-align:left;">

207.2 (54.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

2286.1 (60.3)

</td>

<td style="text-align:left;">

1120.7 (52.6)

</td>

<td style="text-align:left;">

250.4 (48.9)

</td>

<td style="text-align:left;">

157.2 (41.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

215.3 (5.7)

</td>

<td style="text-align:left;">

97.1 (4.6)

</td>

<td style="text-align:left;">

14.3 (2.8)

</td>

<td style="text-align:left;">

15.0 (3.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

42.9 (1.1)

</td>

<td style="text-align:left;">

13.2 (0.6)

</td>

<td style="text-align:left;">

5.3 (1.0)

</td>

<td style="text-align:left;">

2.0 (0.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

safefrst (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

1389.3 (36.9)

</td>

<td style="text-align:left;">

973.9 (46.1)

</td>

<td style="text-align:left;">

246.8 (49.5)

</td>

<td style="text-align:left;">

217.4 (57.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

1984.1 (52.8)

</td>

<td style="text-align:left;">

951.0 (45.0)

</td>

<td style="text-align:left;">

231.0 (46.4)

</td>

<td style="text-align:left;">

131.3 (35.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

317.0 (8.4)

</td>

<td style="text-align:left;">

160.3 (7.6)

</td>

<td style="text-align:left;">

19.5 (3.9)

</td>

<td style="text-align:left;">

24.5 (6.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

70.7 (1.9)

</td>

<td style="text-align:left;">

28.4 (1.3)

</td>

<td style="text-align:left;">

0.9 (0.2)

</td>

<td style="text-align:left;">

2.4 (0.6)

</td>

</tr>

</tbody>

</table>

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">

<caption>

Complex

</caption>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

Less-skilled workers

</th>

<th style="text-align:left;">

More-skilled workers

</th>

<th style="text-align:left;">

Low-level managers

</th>

<th style="text-align:left;">

High-level managers

</th>

<th style="text-align:left;">

Petit bourgeoisie

</th>

<th style="text-align:left;">

Small capitalists

</th>

<th style="text-align:left;">

Large capitalists

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

n

</td>

<td style="text-align:left;">

2501.6

</td>

<td style="text-align:left;">

2144.4

</td>

<td style="text-align:left;">

1869.4

</td>

<td style="text-align:left;">

699.7

</td>

<td style="text-align:left;">

664.2

</td>

<td style="text-align:left;">

271.6

</td>

<td style="text-align:left;">

205.8

</td>

</tr>

<tr>

<td style="text-align:left;">

mustwork = no (%)

</td>

<td style="text-align:left;">

1447.3 (72.7)

</td>

<td style="text-align:left;">

1379.9 (78.2)

</td>

<td style="text-align:left;">

1073.6 (69.3)

</td>

<td style="text-align:left;">

407.0 (71.3)

</td>

<td style="text-align:left;">

395.6 (76.2)

</td>

<td style="text-align:left;">

155.3 (71.3)

</td>

<td style="text-align:left;">

89.9 (57.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

chngtme (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

often

</td>

<td style="text-align:left;">

255.3 (16.2)

</td>

<td style="text-align:left;">

407.4 (29.5)

</td>

<td style="text-align:left;">

394.6 (31.2)

</td>

<td style="text-align:left;">

197.3 (46.8)

</td>

<td style="text-align:left;">

316.7 (75.7)

</td>

<td style="text-align:left;">

132.4 (69.2)

</td>

<td style="text-align:left;">

95.6 (77.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

sometimes

</td>

<td style="text-align:left;">

355.0 (22.5)

</td>

<td style="text-align:left;">

266.1 (19.3)

</td>

<td style="text-align:left;">

282.6 (22.3)

</td>

<td style="text-align:left;">

101.3 (24.0)

</td>

<td style="text-align:left;">

48.1 (11.5)

</td>

<td style="text-align:left;">

26.8 (14.0)

</td>

<td style="text-align:left;">

15.2 (12.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

rarely

</td>

<td style="text-align:left;">

284.8 (18.1)

</td>

<td style="text-align:left;">

238.9 (17.3)

</td>

<td style="text-align:left;">

224.5 (17.7)

</td>

<td style="text-align:left;">

45.6 (10.8)

</td>

<td style="text-align:left;">

20.7 (4.9)

</td>

<td style="text-align:left;">

10.2 (5.3)

</td>

<td style="text-align:left;">

5.9 (4.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

never

</td>

<td style="text-align:left;">

681.4 (43.2)

</td>

<td style="text-align:left;">

468.1 (33.9)

</td>

<td style="text-align:left;">

364.2 (28.8)

</td>

<td style="text-align:left;">

77.1 (18.3)

</td>

<td style="text-align:left;">

32.8 (7.8)

</td>

<td style="text-align:left;">

22.1 (11.5)

</td>

<td style="text-align:left;">

6.6 (5.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

famwkoff (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all hard

</td>

<td style="text-align:left;">

808.9 (40.4)

</td>

<td style="text-align:left;">

765.0 (42.8)

</td>

<td style="text-align:left;">

622.1 (40.0)

</td>

<td style="text-align:left;">

231.0 (40.5)

</td>

<td style="text-align:left;">

301.6 (57.0)

</td>

<td style="text-align:left;">

91.3 (40.2)

</td>

<td style="text-align:left;">

71.9 (45.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too hard

</td>

<td style="text-align:left;">

601.8 (30.0)

</td>

<td style="text-align:left;">

608.8 (34.1)

</td>

<td style="text-align:left;">

500.6 (32.2)

</td>

<td style="text-align:left;">

179.1 (31.4)

</td>

<td style="text-align:left;">

113.4 (21.4)

</td>

<td style="text-align:left;">

52.3 (23.0)

</td>

<td style="text-align:left;">

48.1 (30.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat hard

</td>

<td style="text-align:left;">

365.9 (18.3)

</td>

<td style="text-align:left;">

277.1 (15.5)

</td>

<td style="text-align:left;">

264.5 (17.0)

</td>

<td style="text-align:left;">

112.1 (19.6)

</td>

<td style="text-align:left;">

74.5 (14.1)

</td>

<td style="text-align:left;">

43.7 (19.3)

</td>

<td style="text-align:left;">

25.2 (16.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

very hard

</td>

<td style="text-align:left;">

226.6 (11.3)

</td>

<td style="text-align:left;">

136.5 (7.6)

</td>

<td style="text-align:left;">

168.6 (10.8)

</td>

<td style="text-align:left;">

48.5 (8.5)

</td>

<td style="text-align:left;">

40.0 (7.6)

</td>

<td style="text-align:left;">

39.6 (17.4)

</td>

<td style="text-align:left;">

12.9 (8.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

wkvsfam (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

often

</td>

<td style="text-align:left;">

221.7 (11.0)

</td>

<td style="text-align:left;">

151.0 (8.4)

</td>

<td style="text-align:left;">

215.8 (13.9)

</td>

<td style="text-align:left;">

108.4 (19.0)

</td>

<td style="text-align:left;">

57.4 (10.8)

</td>

<td style="text-align:left;">

59.3 (26.2)

</td>

<td style="text-align:left;">

40.7 (25.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

sometimes

</td>

<td style="text-align:left;">

472.9 (23.5)

</td>

<td style="text-align:left;">

548.4 (30.6)

</td>

<td style="text-align:left;">

540.6 (34.7)

</td>

<td style="text-align:left;">

236.0 (41.3)

</td>

<td style="text-align:left;">

159.6 (30.1)

</td>

<td style="text-align:left;">

69.7 (30.8)

</td>

<td style="text-align:left;">

62.2 (39.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

rarely

</td>

<td style="text-align:left;">

617.6 (30.7)

</td>

<td style="text-align:left;">

629.5 (35.2)

</td>

<td style="text-align:left;">

499.8 (32.1)

</td>

<td style="text-align:left;">

141.9 (24.8)

</td>

<td style="text-align:left;">

167.4 (31.6)

</td>

<td style="text-align:left;">

58.1 (25.6)

</td>

<td style="text-align:left;">

43.4 (27.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

never

</td>

<td style="text-align:left;">

700.3 (34.8)

</td>

<td style="text-align:left;">

461.3 (25.8)

</td>

<td style="text-align:left;">

300.2 (19.3)

</td>

<td style="text-align:left;">

85.3 (14.9)

</td>

<td style="text-align:left;">

145.6 (27.5)

</td>

<td style="text-align:left;">

39.3 (17.4)

</td>

<td style="text-align:left;">

13.3 (8.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

secondwk = no (%)

</td>

<td style="text-align:left;">

1712.6 (85.1)

</td>

<td style="text-align:left;">

1479.4 (82.8)

</td>

<td style="text-align:left;">

1274.2 (81.8)

</td>

<td style="text-align:left;">

483.6 (84.5)

</td>

<td style="text-align:left;">

418.2 (78.7)

</td>

<td style="text-align:left;">

184.9 (81.0)

</td>

<td style="text-align:left;">

140.2 (87.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

learnnew (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

550.3 (27.4)

</td>

<td style="text-align:left;">

904.0 (50.5)

</td>

<td style="text-align:left;">

787.7 (50.6)

</td>

<td style="text-align:left;">

308.2 (53.9)

</td>

<td style="text-align:left;">

237.8 (44.8)

</td>

<td style="text-align:left;">

115.8 (51.3)

</td>

<td style="text-align:left;">

87.7 (55.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

971.2 (48.4)

</td>

<td style="text-align:left;">

722.1 (40.3)

</td>

<td style="text-align:left;">

623.4 (40.1)

</td>

<td style="text-align:left;">

239.7 (41.9)

</td>

<td style="text-align:left;">

224.4 (42.3)

</td>

<td style="text-align:left;">

98.2 (43.5)

</td>

<td style="text-align:left;">

67.2 (42.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

373.4 (18.6)

</td>

<td style="text-align:left;">

137.4 (7.7)

</td>

<td style="text-align:left;">

129.9 (8.3)

</td>

<td style="text-align:left;">

22.9 (4.0)

</td>

<td style="text-align:left;">

53.6 (10.1)

</td>

<td style="text-align:left;">

9.1 (4.0)

</td>

<td style="text-align:left;">

3.6 (2.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

112.6 (5.6)

</td>

<td style="text-align:left;">

26.6 (1.5)

</td>

<td style="text-align:left;">

15.1 (1.0)

</td>

<td style="text-align:left;">

1.2 (0.2)

</td>

<td style="text-align:left;">

15.1 (2.8)

</td>

<td style="text-align:left;">

2.6 (1.2)

</td>

<td style="text-align:left;">

1.1 (0.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

workfast (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

448.3 (22.3)

</td>

<td style="text-align:left;">

325.6 (18.3)

</td>

<td style="text-align:left;">

412.9 (26.5)

</td>

<td style="text-align:left;">

144.0 (25.3)

</td>

<td style="text-align:left;">

78.5 (14.8)

</td>

<td style="text-align:left;">

59.1 (26.2)

</td>

<td style="text-align:left;">

42.7 (26.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

856.8 (42.6)

</td>

<td style="text-align:left;">

816.6 (45.8)

</td>

<td style="text-align:left;">

728.1 (46.7)

</td>

<td style="text-align:left;">

295.1 (51.9)

</td>

<td style="text-align:left;">

196.2 (37.0)

</td>

<td style="text-align:left;">

107.9 (47.8)

</td>

<td style="text-align:left;">

70.1 (43.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

625.9 (31.1)

</td>

<td style="text-align:left;">

574.4 (32.2)

</td>

<td style="text-align:left;">

393.2 (25.2)

</td>

<td style="text-align:left;">

126.7 (22.3)

</td>

<td style="text-align:left;">

217.0 (40.9)

</td>

<td style="text-align:left;">

50.6 (22.4)

</td>

<td style="text-align:left;">

44.0 (27.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

78.9 (3.9)

</td>

<td style="text-align:left;">

66.8 (3.7)

</td>

<td style="text-align:left;">

23.6 (1.5)

</td>

<td style="text-align:left;">

3.2 (0.6)

</td>

<td style="text-align:left;">

38.3 (7.2)

</td>

<td style="text-align:left;">

8.0 (3.6)

</td>

<td style="text-align:left;">

2.9 (1.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

wrktime (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

959.8 (47.7)

</td>

<td style="text-align:left;">

757.2 (42.4)

</td>

<td style="text-align:left;">

561.1 (36.0)

</td>

<td style="text-align:left;">

150.6 (26.3)

</td>

<td style="text-align:left;">

330.2 (63.8)

</td>

<td style="text-align:left;">

121.0 (56.2)

</td>

<td style="text-align:left;">

62.2 (39.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

800.3 (39.8)

</td>

<td style="text-align:left;">

742.0 (41.5)

</td>

<td style="text-align:left;">

664.9 (42.7)

</td>

<td style="text-align:left;">

279.7 (48.9)

</td>

<td style="text-align:left;">

149.3 (28.9)

</td>

<td style="text-align:left;">

69.5 (32.2)

</td>

<td style="text-align:left;">

77.0 (49.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

172.1 (8.6)

</td>

<td style="text-align:left;">

212.7 (11.9)

</td>

<td style="text-align:left;">

225.6 (14.5)

</td>

<td style="text-align:left;">

97.6 (17.1)

</td>

<td style="text-align:left;">

29.2 (5.7)

</td>

<td style="text-align:left;">

20.2 (9.4)

</td>

<td style="text-align:left;">

12.8 (8.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

79.2 (3.9)

</td>

<td style="text-align:left;">

74.1 (4.1)

</td>

<td style="text-align:left;">

106.8 (6.9)

</td>

<td style="text-align:left;">

43.6 (7.6)

</td>

<td style="text-align:left;">

8.7 (1.7)

</td>

<td style="text-align:left;">

4.8 (2.2)

</td>

<td style="text-align:left;">

5.3 (3.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

workdiff (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

516.2 (25.7)

</td>

<td style="text-align:left;">

727.0 (40.6)

</td>

<td style="text-align:left;">

742.8 (47.7)

</td>

<td style="text-align:left;">

310.3 (54.3)

</td>

<td style="text-align:left;">

203.5 (38.5)

</td>

<td style="text-align:left;">

110.1 (48.8)

</td>

<td style="text-align:left;">

90.7 (56.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

1135.5 (56.6)

</td>

<td style="text-align:left;">

886.2 (49.5)

</td>

<td style="text-align:left;">

721.4 (46.3)

</td>

<td style="text-align:left;">

245.4 (42.9)

</td>

<td style="text-align:left;">

263.4 (49.8)

</td>

<td style="text-align:left;">

102.7 (45.5)

</td>

<td style="text-align:left;">

62.3 (39.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

303.5 (15.1)

</td>

<td style="text-align:left;">

156.5 (8.7)

</td>

<td style="text-align:left;">

88.1 (5.7)

</td>

<td style="text-align:left;">

15.4 (2.7)

</td>

<td style="text-align:left;">

52.2 (9.9)

</td>

<td style="text-align:left;">

12.9 (5.7)

</td>

<td style="text-align:left;">

4.7 (2.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

49.9 (2.5)

</td>

<td style="text-align:left;">

19.5 (1.1)

</td>

<td style="text-align:left;">

6.1 (0.4)

</td>

<td style="text-align:left;">

0.8 (0.1)

</td>

<td style="text-align:left;">

10.0 (1.9)

</td>

<td style="text-align:left;">

0.0 (0.0)

</td>

<td style="text-align:left;">

2.0 (1.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

toofewwk (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

often

</td>

<td style="text-align:left;">

504.4 (25.2)

</td>

<td style="text-align:left;">

436.2 (24.4)

</td>

<td style="text-align:left;">

458.5 (29.4)

</td>

<td style="text-align:left;">

185.9 (32.6)

</td>

<td style="text-align:left;">

67.9 (13.1)

</td>

<td style="text-align:left;">

46.6 (20.8)

</td>

<td style="text-align:left;">

31.2 (19.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

sometimes

</td>

<td style="text-align:left;">

646.8 (32.3)

</td>

<td style="text-align:left;">

600.8 (33.7)

</td>

<td style="text-align:left;">

590.7 (37.9)

</td>

<td style="text-align:left;">

234.8 (41.2)

</td>

<td style="text-align:left;">

109.2 (21.1)

</td>

<td style="text-align:left;">

79.6 (35.6)

</td>

<td style="text-align:left;">

70.5 (44.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

rarely

</td>

<td style="text-align:left;">

585.0 (29.2)

</td>

<td style="text-align:left;">

541.4 (30.3)

</td>

<td style="text-align:left;">

393.8 (25.3)

</td>

<td style="text-align:left;">

111.8 (19.6)

</td>

<td style="text-align:left;">

136.5 (26.4)

</td>

<td style="text-align:left;">

63.0 (28.2)

</td>

<td style="text-align:left;">

45.1 (28.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

never

</td>

<td style="text-align:left;">

268.8 (13.4)

</td>

<td style="text-align:left;">

206.0 (11.5)

</td>

<td style="text-align:left;">

114.7 (7.4)

</td>

<td style="text-align:left;">

37.9 (6.7)

</td>

<td style="text-align:left;">

203.6 (39.4)

</td>

<td style="text-align:left;">

34.4 (15.4)

</td>

<td style="text-align:left;">

10.4 (6.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

overwork (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

110.7 (5.5)

</td>

<td style="text-align:left;">

136.5 (7.6)

</td>

<td style="text-align:left;">

143.8 (9.2)

</td>

<td style="text-align:left;">

56.1 (9.8)

</td>

<td style="text-align:left;">

17.2 (3.3)

</td>

<td style="text-align:left;">

16.4 (7.3)

</td>

<td style="text-align:left;">

19.1 (12.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

473.4 (23.6)

</td>

<td style="text-align:left;">

367.2 (20.6)

</td>

<td style="text-align:left;">

398.3 (25.6)

</td>

<td style="text-align:left;">

179.7 (31.5)

</td>

<td style="text-align:left;">

92.6 (17.5)

</td>

<td style="text-align:left;">

51.9 (23.2)

</td>

<td style="text-align:left;">

44.2 (27.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

1209.0 (60.3)

</td>

<td style="text-align:left;">

1090.0 (61.0)

</td>

<td style="text-align:left;">

890.3 (57.3)

</td>

<td style="text-align:left;">

286.1 (50.1)

</td>

<td style="text-align:left;">

348.0 (65.8)

</td>

<td style="text-align:left;">

138.4 (61.8)

</td>

<td style="text-align:left;">

79.8 (50.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

212.1 (10.6)

</td>

<td style="text-align:left;">

193.2 (10.8)

</td>

<td style="text-align:left;">

122.5 (7.9)

</td>

<td style="text-align:left;">

49.3 (8.6)

</td>

<td style="text-align:left;">

70.9 (13.4)

</td>

<td style="text-align:left;">

17.5 (7.8)

</td>

<td style="text-align:left;">

16.4 (10.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

myskills (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

516.6 (25.7)

</td>

<td style="text-align:left;">

695.0 (38.8)

</td>

<td style="text-align:left;">

723.3 (46.4)

</td>

<td style="text-align:left;">

262.8 (46.0)

</td>

<td style="text-align:left;">

262.9 (49.6)

</td>

<td style="text-align:left;">

113.4 (50.3)

</td>

<td style="text-align:left;">

91.1 (57.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

1170.5 (58.3)

</td>

<td style="text-align:left;">

958.0 (53.5)

</td>

<td style="text-align:left;">

734.2 (47.1)

</td>

<td style="text-align:left;">

282.5 (49.5)

</td>

<td style="text-align:left;">

236.1 (44.5)

</td>

<td style="text-align:left;">

98.9 (43.8)

</td>

<td style="text-align:left;">

64.7 (40.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

260.3 (13.0)

</td>

<td style="text-align:left;">

112.2 (6.3)

</td>

<td style="text-align:left;">

86.9 (5.6)

</td>

<td style="text-align:left;">

24.9 (4.4)

</td>

<td style="text-align:left;">

24.0 (4.5)

</td>

<td style="text-align:left;">

12.3 (5.4)

</td>

<td style="text-align:left;">

2.7 (1.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

61.5 (3.1)

</td>

<td style="text-align:left;">

23.8 (1.3)

</td>

<td style="text-align:left;">

13.9 (0.9)

</td>

<td style="text-align:left;">

1.0 (0.2)

</td>

<td style="text-align:left;">

7.1 (1.3)

</td>

<td style="text-align:left;">

1.1 (0.5)

</td>

<td style="text-align:left;">

1.1 (0.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

trainops (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

477.5 (58.7)

</td>

<td style="text-align:left;">

469.9 (61.7)

</td>

<td style="text-align:left;">

452.6 (61.7)

</td>

<td style="text-align:left;">

146.2 (61.8)

</td>

<td style="text-align:left;">

149.1 (72.1)

</td>

<td style="text-align:left;">

70.6 (79.4)

</td>

<td style="text-align:left;">

52.3 (79.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

259.1 (31.8)

</td>

<td style="text-align:left;">

238.9 (31.4)

</td>

<td style="text-align:left;">

238.4 (32.5)

</td>

<td style="text-align:left;">

70.2 (29.7)

</td>

<td style="text-align:left;">

38.8 (18.8)

</td>

<td style="text-align:left;">

15.8 (17.8)

</td>

<td style="text-align:left;">

12.5 (19.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

48.7 (6.0)

</td>

<td style="text-align:left;">

32.0 (4.2)

</td>

<td style="text-align:left;">

26.6 (3.6)

</td>

<td style="text-align:left;">

15.1 (6.4)

</td>

<td style="text-align:left;">

7.1 (3.5)

</td>

<td style="text-align:left;">

1.7 (1.9)

</td>

<td style="text-align:left;">

1.1 (1.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

28.3 (3.5)

</td>

<td style="text-align:left;">

20.7 (2.7)

</td>

<td style="text-align:left;">

15.6 (2.1)

</td>

<td style="text-align:left;">

4.9 (2.1)

</td>

<td style="text-align:left;">

11.8 (5.7)

</td>

<td style="text-align:left;">

0.9 (1.0)

</td>

<td style="text-align:left;">

0.0 (0.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

opdevel (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

515.0 (25.8)

</td>

<td style="text-align:left;">

642.1 (35.9)

</td>

<td style="text-align:left;">

649.7 (41.9)

</td>

<td style="text-align:left;">

274.0 (47.9)

</td>

<td style="text-align:left;">

332.1 (64.1)

</td>

<td style="text-align:left;">

143.6 (66.4)

</td>

<td style="text-align:left;">

114.4 (72.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

893.4 (44.7)

</td>

<td style="text-align:left;">

801.2 (44.8)

</td>

<td style="text-align:left;">

685.7 (44.2)

</td>

<td style="text-align:left;">

244.2 (42.7)

</td>

<td style="text-align:left;">

129.0 (24.9)

</td>

<td style="text-align:left;">

53.8 (24.9)

</td>

<td style="text-align:left;">

38.3 (24.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

387.4 (19.4)

</td>

<td style="text-align:left;">

249.8 (14.0)

</td>

<td style="text-align:left;">

153.1 (9.9)

</td>

<td style="text-align:left;">

39.3 (6.9)

</td>

<td style="text-align:left;">

32.4 (6.3)

</td>

<td style="text-align:left;">

14.7 (6.8)

</td>

<td style="text-align:left;">

0.9 (0.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

203.7 (10.2)

</td>

<td style="text-align:left;">

95.8 (5.4)

</td>

<td style="text-align:left;">

62.9 (4.1)

</td>

<td style="text-align:left;">

14.0 (2.4)

</td>

<td style="text-align:left;">

24.4 (4.7)

</td>

<td style="text-align:left;">

4.2 (1.9)

</td>

<td style="text-align:left;">

4.5 (2.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

respect (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

595.0 (29.7)

</td>

<td style="text-align:left;">

692.6 (38.7)

</td>

<td style="text-align:left;">

650.3 (41.8)

</td>

<td style="text-align:left;">

241.2 (42.2)

</td>

<td style="text-align:left;">

244.5 (46.6)

</td>

<td style="text-align:left;">

98.9 (44.4)

</td>

<td style="text-align:left;">

88.3 (55.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

1198.9 (59.8)

</td>

<td style="text-align:left;">

974.2 (54.5)

</td>

<td style="text-align:left;">

803.7 (51.6)

</td>

<td style="text-align:left;">

299.3 (52.3)

</td>

<td style="text-align:left;">

258.2 (49.2)

</td>

<td style="text-align:left;">

116.6 (52.3)

</td>

<td style="text-align:left;">

68.5 (43.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

154.2 (7.7)

</td>

<td style="text-align:left;">

98.8 (5.5)

</td>

<td style="text-align:left;">

82.5 (5.3)

</td>

<td style="text-align:left;">

27.6 (4.8)

</td>

<td style="text-align:left;">

20.4 (3.9)

</td>

<td style="text-align:left;">

6.5 (2.9)

</td>

<td style="text-align:left;">

0.5 (0.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

57.8 (2.9)

</td>

<td style="text-align:left;">

23.2 (1.3)

</td>

<td style="text-align:left;">

20.0 (1.3)

</td>

<td style="text-align:left;">

4.0 (0.7)

</td>

<td style="text-align:left;">

2.0 (0.4)

</td>

<td style="text-align:left;">

0.8 (0.4)

</td>

<td style="text-align:left;">

0.9 (0.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

trustman (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

434.8 (21.7)

</td>

<td style="text-align:left;">

466.6 (26.2)

</td>

<td style="text-align:left;">

487.3 (31.4)

</td>

<td style="text-align:left;">

163.7 (28.9)

</td>

<td style="text-align:left;">

275.5 (53.9)

</td>

<td style="text-align:left;">

122.7 (57.5)

</td>

<td style="text-align:left;">

90.9 (58.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

1092.6 (54.5)

</td>

<td style="text-align:left;">

958.7 (53.8)

</td>

<td style="text-align:left;">

767.5 (49.5)

</td>

<td style="text-align:left;">

298.9 (52.7)

</td>

<td style="text-align:left;">

209.2 (40.9)

</td>

<td style="text-align:left;">

79.3 (37.2)

</td>

<td style="text-align:left;">

60.6 (38.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

354.4 (17.7)

</td>

<td style="text-align:left;">

272.4 (15.3)

</td>

<td style="text-align:left;">

223.5 (14.4)

</td>

<td style="text-align:left;">

89.7 (15.8)

</td>

<td style="text-align:left;">

23.3 (4.6)

</td>

<td style="text-align:left;">

8.4 (3.9)

</td>

<td style="text-align:left;">

4.6 (2.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

122.5 (6.1)

</td>

<td style="text-align:left;">

83.7 (4.7)

</td>

<td style="text-align:left;">

73.3 (4.7)

</td>

<td style="text-align:left;">

14.4 (2.5)

</td>

<td style="text-align:left;">

3.2 (0.6)

</td>

<td style="text-align:left;">

3.1 (1.4)

</td>

<td style="text-align:left;">

0.0 (0.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

manvsemp (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very good

</td>

<td style="text-align:left;">

608.5 (30.4)

</td>

<td style="text-align:left;">

578.8 (32.5)

</td>

<td style="text-align:left;">

504.7 (32.4)

</td>

<td style="text-align:left;">

183.1 (32.1)

</td>

<td style="text-align:left;">

270.5 (61.2)

</td>

<td style="text-align:left;">

140.7 (64.2)

</td>

<td style="text-align:left;">

87.7 (56.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

quite good

</td>

<td style="text-align:left;">

752.4 (37.5)

</td>

<td style="text-align:left;">

655.9 (36.8)

</td>

<td style="text-align:left;">

618.3 (39.8)

</td>

<td style="text-align:left;">

253.6 (44.4)

</td>

<td style="text-align:left;">

91.2 (20.6)

</td>

<td style="text-align:left;">

65.2 (29.7)

</td>

<td style="text-align:left;">

49.6 (31.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

neither good nor bad

</td>

<td style="text-align:left;">

458.4 (22.9)

</td>

<td style="text-align:left;">

414.1 (23.2)

</td>

<td style="text-align:left;">

325.5 (20.9)

</td>

<td style="text-align:left;">

100.0 (17.5)

</td>

<td style="text-align:left;">

70.4 (15.9)

</td>

<td style="text-align:left;">

10.0 (4.5)

</td>

<td style="text-align:left;">

17.8 (11.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

quite bad

</td>

<td style="text-align:left;">

131.4 (6.6)

</td>

<td style="text-align:left;">

110.7 (6.2)

</td>

<td style="text-align:left;">

81.1 (5.2)

</td>

<td style="text-align:left;">

29.5 (5.2)

</td>

<td style="text-align:left;">

7.6 (1.7)

</td>

<td style="text-align:left;">

2.3 (1.0)

</td>

<td style="text-align:left;">

1.4 (0.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

very bad

</td>

<td style="text-align:left;">

53.9 (2.7)

</td>

<td style="text-align:left;">

21.7 (1.2)

</td>

<td style="text-align:left;">

25.9 (1.7)

</td>

<td style="text-align:left;">

4.9 (0.9)

</td>

<td style="text-align:left;">

2.7 (0.6)

</td>

<td style="text-align:left;">

1.1 (0.5)

</td>

<td style="text-align:left;">

0.0 (0.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

suphelp (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

920.2 (45.9)

</td>

<td style="text-align:left;">

882.9 (49.7)

</td>

<td style="text-align:left;">

784.3 (50.6)

</td>

<td style="text-align:left;">

261.1 (46.2)

</td>

<td style="text-align:left;">

253.3 (59.5)

</td>

<td style="text-align:left;">

121.7 (67.5)

</td>

<td style="text-align:left;">

78.9 (63.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

768.6 (38.3)

</td>

<td style="text-align:left;">

662.8 (37.3)

</td>

<td style="text-align:left;">

532.1 (34.3)

</td>

<td style="text-align:left;">

231.9 (41.0)

</td>

<td style="text-align:left;">

94.6 (22.2)

</td>

<td style="text-align:left;">

34.4 (19.1)

</td>

<td style="text-align:left;">

35.3 (28.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

191.8 (9.6)

</td>

<td style="text-align:left;">

146.7 (8.3)

</td>

<td style="text-align:left;">

140.6 (9.1)

</td>

<td style="text-align:left;">

51.7 (9.1)

</td>

<td style="text-align:left;">

25.3 (5.9)

</td>

<td style="text-align:left;">

13.2 (7.3)

</td>

<td style="text-align:left;">

7.2 (5.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

124.0 (6.2)

</td>

<td style="text-align:left;">

84.8 (4.8)

</td>

<td style="text-align:left;">

92.2 (6.0)

</td>

<td style="text-align:left;">

20.8 (3.7)

</td>

<td style="text-align:left;">

52.3 (12.3)

</td>

<td style="text-align:left;">

11.1 (6.1)

</td>

<td style="text-align:left;">

2.4 (1.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

supcares (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

852.0 (42.8)

</td>

<td style="text-align:left;">

938.4 (52.8)

</td>

<td style="text-align:left;">

823.3 (53.2)

</td>

<td style="text-align:left;">

302.6 (53.4)

</td>

<td style="text-align:left;">

234.4 (56.1)

</td>

<td style="text-align:left;">

120.6 (66.7)

</td>

<td style="text-align:left;">

93.5 (71.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

756.9 (38.0)

</td>

<td style="text-align:left;">

611.0 (34.4)

</td>

<td style="text-align:left;">

504.8 (32.6)

</td>

<td style="text-align:left;">

183.2 (32.3)

</td>

<td style="text-align:left;">

92.8 (22.2)

</td>

<td style="text-align:left;">

28.8 (15.9)

</td>

<td style="text-align:left;">

29.0 (22.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

224.6 (11.3)

</td>

<td style="text-align:left;">

143.0 (8.0)

</td>

<td style="text-align:left;">

133.4 (8.6)

</td>

<td style="text-align:left;">

55.3 (9.8)

</td>

<td style="text-align:left;">

25.0 (6.0)

</td>

<td style="text-align:left;">

12.8 (7.1)

</td>

<td style="text-align:left;">

4.1 (3.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

156.0 (7.8)

</td>

<td style="text-align:left;">

85.1 (4.8)

</td>

<td style="text-align:left;">

87.4 (5.6)

</td>

<td style="text-align:left;">

25.5 (4.5)

</td>

<td style="text-align:left;">

65.7 (15.7)

</td>

<td style="text-align:left;">

18.5 (10.3)

</td>

<td style="text-align:left;">

4.9 (3.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

wkfreedm (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

868.5 (43.3)

</td>

<td style="text-align:left;">

936.8 (52.3)

</td>

<td style="text-align:left;">

882.9 (56.7)

</td>

<td style="text-align:left;">

360.4 (63.0)

</td>

<td style="text-align:left;">

447.9 (86.4)

</td>

<td style="text-align:left;">

179.8 (82.1)

</td>

<td style="text-align:left;">

137.8 (87.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

754.7 (37.6)

</td>

<td style="text-align:left;">

624.7 (34.9)

</td>

<td style="text-align:left;">

517.4 (33.2)

</td>

<td style="text-align:left;">

165.0 (28.9)

</td>

<td style="text-align:left;">

50.9 (9.8)

</td>

<td style="text-align:left;">

35.3 (16.1)

</td>

<td style="text-align:left;">

19.3 (12.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

262.3 (13.1)

</td>

<td style="text-align:left;">

163.9 (9.2)

</td>

<td style="text-align:left;">

120.2 (7.7)

</td>

<td style="text-align:left;">

32.5 (5.7)

</td>

<td style="text-align:left;">

10.5 (2.0)

</td>

<td style="text-align:left;">

2.9 (1.3)

</td>

<td style="text-align:left;">

0.0 (0.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

122.4 (6.1)

</td>

<td style="text-align:left;">

64.3 (3.6)

</td>

<td style="text-align:left;">

36.1 (2.3)

</td>

<td style="text-align:left;">

13.7 (2.4)

</td>

<td style="text-align:left;">

9.1 (1.7)

</td>

<td style="text-align:left;">

0.9 (0.4)

</td>

<td style="text-align:left;">

0.0 (0.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

lotofsay (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

110.5 (13.6)

</td>

<td style="text-align:left;">

155.6 (20.4)

</td>

<td style="text-align:left;">

276.6 (37.6)

</td>

<td style="text-align:left;">

107.0 (45.2)

</td>

<td style="text-align:left;">

131.5 (58.4)

</td>

<td style="text-align:left;">

63.4 (63.8)

</td>

<td style="text-align:left;">

54.8 (80.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

321.3 (39.5)

</td>

<td style="text-align:left;">

342.2 (44.9)

</td>

<td style="text-align:left;">

328.9 (44.7)

</td>

<td style="text-align:left;">

103.2 (43.6)

</td>

<td style="text-align:left;">

74.8 (33.2)

</td>

<td style="text-align:left;">

31.6 (31.8)

</td>

<td style="text-align:left;">

13.1 (19.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

295.7 (36.3)

</td>

<td style="text-align:left;">

202.2 (26.5)

</td>

<td style="text-align:left;">

107.4 (14.6)

</td>

<td style="text-align:left;">

25.3 (10.7)

</td>

<td style="text-align:left;">

16.7 (7.4)

</td>

<td style="text-align:left;">

4.4 (4.4)

</td>

<td style="text-align:left;">

0.0 (0.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

86.3 (10.6)

</td>

<td style="text-align:left;">

61.6 (8.1)

</td>

<td style="text-align:left;">

23.5 (3.2)

</td>

<td style="text-align:left;">

1.1 (0.5)

</td>

<td style="text-align:left;">

2.3 (1.0)

</td>

<td style="text-align:left;">

0.0 (0.0)

</td>

<td style="text-align:left;">

0.0 (0.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

wkdecide (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

often

</td>

<td style="text-align:left;">

529.6 (26.3)

</td>

<td style="text-align:left;">

613.3 (34.3)

</td>

<td style="text-align:left;">

790.8 (50.8)

</td>

<td style="text-align:left;">

356.0 (62.3)

</td>

<td style="text-align:left;">

181.9 (34.7)

</td>

<td style="text-align:left;">

117.2 (53.3)

</td>

<td style="text-align:left;">

100.2 (63.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

sometimes

</td>

<td style="text-align:left;">

814.5 (40.5)

</td>

<td style="text-align:left;">

759.6 (42.5)

</td>

<td style="text-align:left;">

591.6 (38.0)

</td>

<td style="text-align:left;">

162.8 (28.5)

</td>

<td style="text-align:left;">

130.5 (24.9)

</td>

<td style="text-align:left;">

56.5 (25.7)

</td>

<td style="text-align:left;">

43.3 (27.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

rarely

</td>

<td style="text-align:left;">

396.7 (19.7)

</td>

<td style="text-align:left;">

300.2 (16.8)

</td>

<td style="text-align:left;">

130.9 (8.4)

</td>

<td style="text-align:left;">

28.4 (5.0)

</td>

<td style="text-align:left;">

85.2 (16.3)

</td>

<td style="text-align:left;">

31.2 (14.2)

</td>

<td style="text-align:left;">

10.0 (6.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

never

</td>

<td style="text-align:left;">

271.4 (13.5)

</td>

<td style="text-align:left;">

113.3 (6.3)

</td>

<td style="text-align:left;">

44.0 (2.8)

</td>

<td style="text-align:left;">

24.4 (4.3)

</td>

<td style="text-align:left;">

126.4 (24.1)

</td>

<td style="text-align:left;">

14.9 (6.8)

</td>

<td style="text-align:left;">

4.6 (2.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

satjob1 (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very satisfied

</td>

<td style="text-align:left;">

787.3 (39.2)

</td>

<td style="text-align:left;">

886.7 (49.6)

</td>

<td style="text-align:left;">

733.5 (47.2)

</td>

<td style="text-align:left;">

308.8 (54.0)

</td>

<td style="text-align:left;">

329.3 (62.4)

</td>

<td style="text-align:left;">

146.4 (64.1)

</td>

<td style="text-align:left;">

105.8 (65.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat satisfied

</td>

<td style="text-align:left;">

921.9 (45.9)

</td>

<td style="text-align:left;">

737.8 (41.3)

</td>

<td style="text-align:left;">

685.7 (44.1)

</td>

<td style="text-align:left;">

213.3 (37.3)

</td>

<td style="text-align:left;">

167.9 (31.8)

</td>

<td style="text-align:left;">

66.6 (29.2)

</td>

<td style="text-align:left;">

52.2 (32.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too satisfied

</td>

<td style="text-align:left;">

193.9 (9.7)

</td>

<td style="text-align:left;">

121.8 (6.8)

</td>

<td style="text-align:left;">

108.8 (7.0)

</td>

<td style="text-align:left;">

42.7 (7.5)

</td>

<td style="text-align:left;">

19.6 (3.7)

</td>

<td style="text-align:left;">

6.8 (3.0)

</td>

<td style="text-align:left;">

1.4 (0.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all satisfied

</td>

<td style="text-align:left;">

105.4 (5.2)

</td>

<td style="text-align:left;">

40.5 (2.3)

</td>

<td style="text-align:left;">

26.8 (1.7)

</td>

<td style="text-align:left;">

7.3 (1.3)

</td>

<td style="text-align:left;">

10.8 (2.1)

</td>

<td style="text-align:left;">

8.5 (3.7)

</td>

<td style="text-align:left;">

1.1 (0.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

fairearn (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

much less than you deserve

</td>

<td style="text-align:left;">

286.1 (14.5)

</td>

<td style="text-align:left;">

204.9 (11.6)

</td>

<td style="text-align:left;">

209.8 (13.6)

</td>

<td style="text-align:left;">

50.9 (9.0)

</td>

<td style="text-align:left;">

44.3 (9.0)

</td>

<td style="text-align:left;">

25.8 (12.4)

</td>

<td style="text-align:left;">

9.1 (6.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat less than you deserve

</td>

<td style="text-align:left;">

551.4 (28.0)

</td>

<td style="text-align:left;">

535.3 (30.4)

</td>

<td style="text-align:left;">

484.8 (31.5)

</td>

<td style="text-align:left;">

190.2 (33.7)

</td>

<td style="text-align:left;">

81.0 (16.4)

</td>

<td style="text-align:left;">

31.8 (15.2)

</td>

<td style="text-align:left;">

32.4 (21.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

about as much as you deserve

</td>

<td style="text-align:left;">

969.7 (49.2)

</td>

<td style="text-align:left;">

890.8 (50.5)

</td>

<td style="text-align:left;">

714.9 (46.4)

</td>

<td style="text-align:left;">

264.4 (46.9)

</td>

<td style="text-align:left;">

309.6 (62.6)

</td>

<td style="text-align:left;">

129.7 (62.1)

</td>

<td style="text-align:left;">

93.4 (61.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat more than you deserve

</td>

<td style="text-align:left;">

128.9 (6.5)

</td>

<td style="text-align:left;">

109.2 (6.2)

</td>

<td style="text-align:left;">

112.5 (7.3)

</td>

<td style="text-align:left;">

43.0 (7.6)

</td>

<td style="text-align:left;">

47.8 (9.7)

</td>

<td style="text-align:left;">

14.7 (7.1)

</td>

<td style="text-align:left;">

9.8 (6.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

much more than you deserve

</td>

<td style="text-align:left;">

35.7 (1.8)

</td>

<td style="text-align:left;">

23.5 (1.3)

</td>

<td style="text-align:left;">

18.5 (1.2)

</td>

<td style="text-align:left;">

15.5 (2.7)

</td>

<td style="text-align:left;">

11.5 (2.3)

</td>

<td style="text-align:left;">

6.6 (3.2)

</td>

<td style="text-align:left;">

8.0 (5.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

fringeok (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

595.6 (29.9)

</td>

<td style="text-align:left;">

719.8 (40.8)

</td>

<td style="text-align:left;">

595.0 (38.4)

</td>

<td style="text-align:left;">

253.5 (44.5)

</td>

<td style="text-align:left;">

178.9 (35.3)

</td>

<td style="text-align:left;">

100.0 (47.2)

</td>

<td style="text-align:left;">

82.6 (52.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

734.4 (36.9)

</td>

<td style="text-align:left;">

615.1 (34.9)

</td>

<td style="text-align:left;">

548.2 (35.4)

</td>

<td style="text-align:left;">

226.5 (39.8)

</td>

<td style="text-align:left;">

117.8 (23.2)

</td>

<td style="text-align:left;">

47.8 (22.6)

</td>

<td style="text-align:left;">

43.3 (27.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

260.3 (13.1)

</td>

<td style="text-align:left;">

200.6 (11.4)

</td>

<td style="text-align:left;">

194.3 (12.5)

</td>

<td style="text-align:left;">

54.5 (9.6)

</td>

<td style="text-align:left;">

57.8 (11.4)

</td>

<td style="text-align:left;">

24.3 (11.5)

</td>

<td style="text-align:left;">

15.8 (10.1)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

400.5 (20.1)

</td>

<td style="text-align:left;">

227.5 (12.9)

</td>

<td style="text-align:left;">

210.7 (13.6)

</td>

<td style="text-align:left;">

35.1 (6.2)

</td>

<td style="text-align:left;">

152.7 (30.1)

</td>

<td style="text-align:left;">

39.5 (18.7)

</td>

<td style="text-align:left;">

15.5 (9.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

rincblls = no (%)

</td>

<td style="text-align:left;">

1262.9 (63.1)

</td>

<td style="text-align:left;">

1023.7 (57.4)

</td>

<td style="text-align:left;">

800.3 (51.5)

</td>

<td style="text-align:left;">

217.3 (38.2)

</td>

<td style="text-align:left;">

287.3 (55.0)

</td>

<td style="text-align:left;">

76.7 (34.3)

</td>

<td style="text-align:left;">

35.1 (22.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

laidoff = no (%)

</td>

<td style="text-align:left;">

1832.6 (91.2)

</td>

<td style="text-align:left;">

1705.1 (95.3)

</td>

<td style="text-align:left;">

1450.2 (93.1)

</td>

<td style="text-align:left;">

550.7 (96.3)

</td>

<td style="text-align:left;">

471.9 (89.9)

</td>

<td style="text-align:left;">

212.1 (94.2)

</td>

<td style="text-align:left;">

153.5 (96.5)

</td>

</tr>

<tr>

<td style="text-align:left;">

jobsecok (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

very true

</td>

<td style="text-align:left;">

1000.4 (49.9)

</td>

<td style="text-align:left;">

953.7 (53.7)

</td>

<td style="text-align:left;">

918.2 (59.2)

</td>

<td style="text-align:left;">

325.5 (57.0)

</td>

<td style="text-align:left;">

292.2 (57.0)

</td>

<td style="text-align:left;">

137.1 (64.1)

</td>

<td style="text-align:left;">

106.4 (68.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

somewhat true

</td>

<td style="text-align:left;">

696.5 (34.8)

</td>

<td style="text-align:left;">

602.7 (34.0)

</td>

<td style="text-align:left;">

462.3 (29.8)

</td>

<td style="text-align:left;">

187.1 (32.8)

</td>

<td style="text-align:left;">

128.4 (25.1)

</td>

<td style="text-align:left;">

55.6 (26.0)

</td>

<td style="text-align:left;">

32.6 (20.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

not too true

</td>

<td style="text-align:left;">

173.5 (8.7)

</td>

<td style="text-align:left;">

136.9 (7.7)

</td>

<td style="text-align:left;">

112.9 (7.3)

</td>

<td style="text-align:left;">

39.5 (6.9)

</td>

<td style="text-align:left;">

52.0 (10.1)

</td>

<td style="text-align:left;">

9.3 (4.3)

</td>

<td style="text-align:left;">

12.2 (7.8)

</td>

</tr>

<tr>

<td style="text-align:left;">

not at all true

</td>

<td style="text-align:left;">

132.5 (6.6)

</td>

<td style="text-align:left;">

81.8 (4.6)

</td>

<td style="text-align:left;">

57.0 (3.7)

</td>

<td style="text-align:left;">

19.0 (3.3)

</td>

<td style="text-align:left;">

39.9 (7.8)

</td>

<td style="text-align:left;">

12.0 (5.6)

</td>

<td style="text-align:left;">

5.2 (3.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

any\_discrim\_harass = yes (%)

</td>

<td style="text-align:left;">

438.7 (21.8)

</td>

<td style="text-align:left;">

348.8 (19.5)

</td>

<td style="text-align:left;">

371.8 (23.9)

</td>

<td style="text-align:left;">

132.3 (23.2)

</td>

<td style="text-align:left;">

72.6 (13.9)

</td>

<td style="text-align:left;">

45.3 (20.4)

</td>

<td style="text-align:left;">

17.7 (11.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

safetywk (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

722.9 (36.2)

</td>

<td style="text-align:left;">

692.3 (39.1)

</td>

<td style="text-align:left;">

742.8 (47.9)

</td>

<td style="text-align:left;">

288.0 (50.3)

</td>

<td style="text-align:left;">

251.0 (50.7)

</td>

<td style="text-align:left;">

131.7 (60.1)

</td>

<td style="text-align:left;">

101.6 (64.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

1064.3 (53.2)

</td>

<td style="text-align:left;">

929.0 (52.5)

</td>

<td style="text-align:left;">

673.7 (43.4)

</td>

<td style="text-align:left;">

252.8 (44.2)

</td>

<td style="text-align:left;">

227.0 (45.9)

</td>

<td style="text-align:left;">

76.0 (34.7)

</td>

<td style="text-align:left;">

52.4 (33.2)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

169.3 (8.5)

</td>

<td style="text-align:left;">

130.7 (7.4)

</td>

<td style="text-align:left;">

111.0 (7.2)

</td>

<td style="text-align:left;">

26.8 (4.7)

</td>

<td style="text-align:left;">

13.8 (2.8)

</td>

<td style="text-align:left;">

9.2 (4.2)

</td>

<td style="text-align:left;">

3.7 (2.3)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

42.6 (2.1)

</td>

<td style="text-align:left;">

18.9 (1.1)

</td>

<td style="text-align:left;">

23.2 (1.5)

</td>

<td style="text-align:left;">

4.5 (0.8)

</td>

<td style="text-align:left;">

3.2 (0.6)

</td>

<td style="text-align:left;">

2.2 (1.0)

</td>

<td style="text-align:left;">

0.0 (0.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

safehlth (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

610.6 (30.5)

</td>

<td style="text-align:left;">

636.5 (35.7)

</td>

<td style="text-align:left;">

640.2 (41.1)

</td>

<td style="text-align:left;">

260.6 (45.6)

</td>

<td style="text-align:left;">

242.0 (47.3)

</td>

<td style="text-align:left;">

120.4 (54.2)

</td>

<td style="text-align:left;">

86.8 (55.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

1241.9 (62.0)

</td>

<td style="text-align:left;">

1039.7 (58.3)

</td>

<td style="text-align:left;">

827.0 (53.1)

</td>

<td style="text-align:left;">

289.8 (50.7)

</td>

<td style="text-align:left;">

250.4 (48.9)

</td>

<td style="text-align:left;">

87.4 (39.3)

</td>

<td style="text-align:left;">

68.4 (43.4)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

126.9 (6.3)

</td>

<td style="text-align:left;">

88.4 (5.0)

</td>

<td style="text-align:left;">

79.5 (5.1)

</td>

<td style="text-align:left;">

17.7 (3.1)

</td>

<td style="text-align:left;">

14.3 (2.8)

</td>

<td style="text-align:left;">

12.5 (5.6)

</td>

<td style="text-align:left;">

2.5 (1.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

25.0 (1.2)

</td>

<td style="text-align:left;">

18.0 (1.0)

</td>

<td style="text-align:left;">

9.7 (0.6)

</td>

<td style="text-align:left;">

3.5 (0.6)

</td>

<td style="text-align:left;">

5.3 (1.0)

</td>

<td style="text-align:left;">

2.0 (0.9)

</td>

<td style="text-align:left;">

0.0 (0.0)

</td>

</tr>

<tr>

<td style="text-align:left;">

safefrst (%)

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly agree

</td>

<td style="text-align:left;">

699.4 (35.2)

</td>

<td style="text-align:left;">

689.0 (39.0)

</td>

<td style="text-align:left;">

690.1 (44.8)

</td>

<td style="text-align:left;">

283.8 (50.0)

</td>

<td style="text-align:left;">

246.8 (49.5)

</td>

<td style="text-align:left;">

121.4 (56.1)

</td>

<td style="text-align:left;">

96.0 (60.9)

</td>

</tr>

<tr>

<td style="text-align:left;">

agree

</td>

<td style="text-align:left;">

1070.9 (53.8)

</td>

<td style="text-align:left;">

908.7 (51.4)

</td>

<td style="text-align:left;">

696.7 (45.2)

</td>

<td style="text-align:left;">

250.5 (44.1)

</td>

<td style="text-align:left;">

231.0 (46.4)

</td>

<td style="text-align:left;">

78.3 (36.2)

</td>

<td style="text-align:left;">

51.6 (32.7)

</td>

</tr>

<tr>

<td style="text-align:left;">

disagree

</td>

<td style="text-align:left;">

178.2 (9.0)

</td>

<td style="text-align:left;">

138.8 (7.9)

</td>

<td style="text-align:left;">

131.6 (8.5)

</td>

<td style="text-align:left;">

28.7 (5.1)

</td>

<td style="text-align:left;">

19.5 (3.9)

</td>

<td style="text-align:left;">

15.7 (7.2)

</td>

<td style="text-align:left;">

8.8 (5.6)

</td>

</tr>

<tr>

<td style="text-align:left;">

strongly disagree

</td>

<td style="text-align:left;">

41.0 (2.1)

</td>

<td style="text-align:left;">

29.7 (1.7)

</td>

<td style="text-align:left;">

23.5 (1.5)

</td>

<td style="text-align:left;">

4.8 (0.8)

</td>

<td style="text-align:left;">

0.9 (0.2)

</td>

<td style="text-align:left;">

1.1 (0.5)

</td>

<td style="text-align:left;">

1.3 (0.8)

</td>

</tr>

</tbody>

</table>
