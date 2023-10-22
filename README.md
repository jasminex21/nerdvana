# nerdvana

## Inspiration✨

As a student aiming to break into the tech field in the future, I understand how difficult it is to stay on track and excel in the fast-paced world of computer science. The challenges are real - from managing coursework, staying motivated, to securing internships that can jumpstart your career - it can all get overwhelming. That's why I was inspired to create Nerdvana: I wanted to build a solution that would help fellow students like myself overcome these hurdles and streamline their path to success. 

## What it does🤖

When I built Nerdvana, I wanted it to serve as a home base for students gearing for the tech industry such as myself. 

Something I struggled with during my first year in college was internships - applying, keeping track of responses, and gaining insight from rejections. So, one of Nerdvana's features is a comprehensive internship tracker, that allows users to input each internship (along with key details such as the position name, company, and so on) they've applied to, track acceptances/rejections, and to view their overall statistics for the internship cycle. 

The demands of academics and internship hunting can take a toll on mental well-being. And sometimes, emotions and moods may be difficult to articulate - so Nerdvana provides a mood tracker that allows a user to input their mood anytime in the day, and to track their mood throughout the day. This feature helps users stay aware of their mental state and encourages better self-care.

## How I built Nerdvana🎩

Nerdvana was built entirely in R using the R Shiny package - which is an unconventional, yet underrated means of web app development. 

One commonly-faced issue in the development of Shiny apps is persistent data storage. And when it comes to tracking your internships, it'd probably be ideal if they didn't disappear with each refresh. So, MongoDB Atlas served as the database system where all internship applications and their corresponding statuses were securely stored. This ensured that users' internship data remained intact and accessible across sessions. MongoDB was also used to store moods. 

## Challenges I ran into😱

The biggest challenge I encountered this weekend was learning how to use MongoDB Atlas, and to integrate it into my application as my method of data storage. This was my first time working with MongoDB, so it was an intimidating learning curve. 

## Accomplishments that I'm proud of🏆

I am most proud of having learned how to use MongoDB in the span of a quick weekend. While there is undoubtedly much to learn in the realm of MongoDB, I'm really happy that I was resourceful enough to grasp the skillset that I needed to bring Nerdvana to life. 

## What's next for Nerdvana❓

There are many additional features that I didn't get the time to implement into Nerdvana this weekend: 

- A resources tab to go with the internship tracker, which includes links to job sites, interview tips, and so on
- Applying machine learning to determine likely success rates for internship applications, based on the date of application, tags, and keywords on the job description and whether the application was successful
