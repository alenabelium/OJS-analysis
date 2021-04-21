SELECT edit_decisions.article_id, setting_value as title, decision, date_decided from edit_decisions 
   JOIN article_settings on edit_decisions.article_id = article_settings.article_id 
   LIMIT 1;


   WHERE setting_name = "title";


SELECT * from published_articles
    LEFT JOIN articles ON articles.article_id = published_articles.article_id
    LEFT JOIN authors ON articles.article_id = authors.submission_id
    LEFT JOIN edit_decisions ON edit_decisions.article_id = articles.article_id  
    LEFT JOIN article_settings on article_settings.article_id = articles.article_id
    LEFT JOIN issues on published_articles.issue_id = issues.issue_id;


mysql ojs -u 'ojs' --password="oosaiJuChahvib6Ikexi" -B < published_articles.sql > published_articles.csv
mysql ojs -u 'ojs' --password="oosaiJuChahvib6Ikexi" -B < articles.sql > articles.csv
mysql ojs -u 'ojs' --password="oosaiJuChahvib6Ikexi" -B < authors.sql > authors.csv
mysql ojs -u 'ojs' --password="oosaiJuChahvib6Ikexi" -B < edit_decisions.sql > edit_decisions.csv
mysql ojs -u 'ojs' --password="oosaiJuChahvib6Ikexi" -B < article_settings.sql > article_settings.csv
mysql ojs -u 'ojs' --password="oosaiJuChahvib6Ikexi" -B < issues.sql > issues.csv

SELECT * from $1;


select * from articles limit 10 into outfile 'tmp.csv' FIELDS TERMINATED BY ',' ENCLOSED BY '"' LINES TERMINATED BY '\n';

show columns from published_articles;
+----------------------+------------+------+-----+---------+----------------+
| Field                | Type       | Null | Key | Default | Extra          |
+----------------------+------------+------+-----+---------+----------------+
| published_article_id | bigint(20) | NO   | PRI | NULL    | auto_increment |
| article_id           | bigint(20) | NO   | UNI | NULL    |                |
| issue_id             | bigint(20) | NO   | MUL | NULL    |                |
| date_published       | datetime   | YES  |     | NULL    |                |
| seq                  | double     | NO   |     | 0       |                |
| access_status        | tinyint(4) | NO   |     | 0       |                |
+----------------------+------------+------+-----+---------+----------------+

show columns from articles;
+----------------------+--------------+------+-----+---------+----------------+
| Field                | Type         | Null | Key | Default | Extra          |
+----------------------+--------------+------+-----+---------+----------------+
| article_id           | bigint(20)   | NO   | PRI | NULL    | auto_increment |
| locale               | varchar(5)   | YES  |     | NULL    |                |
| user_id              | bigint(20)   | NO   | MUL | NULL    |                |
| journal_id           | bigint(20)   | NO   | MUL | NULL    |                |
| section_id           | bigint(20)   | YES  | MUL | NULL    |                |
| language             | varchar(10)  | YES  |     | en      |                |
| comments_to_ed       | text         | YES  |     | NULL    |                |
| citations            | text         | YES  |     | NULL    |                |
| date_submitted       | datetime     | YES  |     | NULL    |                |
| last_modified        | datetime     | YES  |     | NULL    |                |
| date_status_modified | datetime     | YES  |     | NULL    |                |
| status               | tinyint(4)   | NO   |     | 1       |                |
| submission_progress  | tinyint(4)   | NO   |     | 1       |                |
| current_round        | tinyint(4)   | NO   |     | 1       |                |
| submission_file_id   | bigint(20)   | YES  |     | NULL    |                |
| revised_file_id      | bigint(20)   | YES  |     | NULL    |                |
| review_file_id       | bigint(20)   | YES  |     | NULL    |                |
| editor_file_id       | bigint(20)   | YES  |     | NULL    |                |
| pages                | varchar(255) | YES  |     | NULL    |                |
| fast_tracked         | tinyint(4)   | NO   |     | 0       |                |
| hide_author          | tinyint(4)   | NO   |     | 0       |                |
| comments_status      | tinyint(4)   | NO   |     | 0       |                |
+----------------------+--------------+------+-----+---------+----------------+


show columns from authors;
+-----------------+--------------+------+-----+---------+----------------+
| Field           | Type         | Null | Key | Default | Extra          |
+-----------------+--------------+------+-----+---------+----------------+
| author_id       | bigint(20)   | NO   | PRI | NULL    | auto_increment |
| submission_id   | bigint(20)   | NO   | MUL | NULL    |                |
| first_name      | varchar(40)  | NO   |     | NULL    |                |
| middle_name     | varchar(40)  | YES  |     | NULL    |                |
| last_name       | varchar(90)  | NO   |     | NULL    |                |
| country         | varchar(90)  | YES  |     | NULL    |                |
| email           | varchar(90)  | NO   |     | NULL    |                |
| url             | varchar(255) | YES  |     | NULL    |                |
| primary_contact | tinyint(4)   | NO   |     | 0       |                |
| seq             | double       | NO   |     | 0       |                |
| user_group_id   | bigint(20)   | YES  |     | NULL    |                |
| suffix          | varchar(40)  | YES  |     | NULL    |                |
+-----------------+--------------+------+-----+---------+----------------+


show columns from article_settings;
+---------------+--------------+------+-----+---------+-------+
| Field         | Type         | Null | Key | Default | Extra |
+---------------+--------------+------+-----+---------+-------+
| article_id    | bigint(20)   | NO   | PRI | NULL    |       |
| locale        | varchar(5)   | NO   | PRI |         |       |
| setting_name  | varchar(255) | NO   | PRI | NULL    |       |
| setting_value | text         | YES  |     | NULL    |       |
| setting_type  | varchar(6)   | NO   |     | NULL    |       |
+---------------+--------------+------+-----+---------+-------+


select distinct setting_name from article_settings;
+-----------------------+
| setting_name          |
+-----------------------+
| copyrightYear         |
| licenseURL            |
| pub-id::publisher-id  |
| abstract              |
| cleanTitle            |
| copyrightHolder       |
| discipline            |
| title                 |
| sponsor               |
| subject               |
| subjectClass          |
| doiSuffix             |
| excludeDoi            |
| pub-id::doi           |
| coverPageAltText      |
| hideCoverPageAbstract |
| hideCoverPageToc      |
| showCoverPage         |
| fileName              |
| originalFileName      |
+-----------------------+    

Keywordi:
select setting_value from article_settings where setting_name = 'subject';

select setting_value from article_settings where setting_name = 'subjectClass';



SELECT 
  TABLE_NAME,COLUMN_NAME,CONSTRAINT_NAME, REFERENCED_TABLE_NAME,REFERENCED_COLUMN_NAME
FROM
  INFORMATION_SCHEMA.KEY_COLUMN_USAGE



SELECT DISTINCT s.submission_id, p.setting_value, a.first_name, a.last_name, s.date_submitted, s.last_modified, s.status, s.stage_id, u.first_name, u.last_name, r.recommendation 
    from submissions s, 
    submission_settings p, 
    authors a, 
    users u, 
    review_assignments r 
    
    WHERE p.submission_id = s.submission_id AND a.submission_id = s.submission_id AND u.user_id = r.reviewer_id AND r.submission_id = s.submission_id AND p.setting_name = 'title' GROUP BY s.submission_id;



select articles.article_id, pages from published_articles left join articles on published_articles.article_id = articles.article_id limit 10;


SELECT published_articles.published_article_id, articles.article_id, issue_id, date_published, status, pages FROM published_articles
        LEFT JOIN articles ON articles.article_id = published_articles.article_id LIMIT 10;

SELECT published_articles.published_article_id, articles.article_id, issue_id, date_published, status, pages FROM published_articles
        LEFT JOIN articles ON articles.article_id = published_articles.article_id
        WHERE articles.article_id>=1598;      