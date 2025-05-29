# Agent-based Modeling

## Information

 - Speaker: Dr Milena Tsvetkova
 - Location: LSE COL 1.06
 - Date: May 20, 2025

## Agenda

 - Morning: What is ABM

   - Modeling in social science
   - Characteristics of ABMs
   - Prominent ABMs: Segregation, co-operation (political science) and contagion (epidemiology)
   - Combining ABMs and other methods, such as experiment

 - Afternoon: NetLogo (https://ccl.northwestern.edu/netlogo)

## Introduction

 - Modeling is an endeavor of abstract simplification: Statistical (including ML), analytical (game theory), and agent based

   - Statistical modeling: focusing on summarising data but not so much the process
   - Analytical modeling: focusing on explaining the process but not necessarily data
   - Agent-based models: Agents made by computers with certain properties interact with each other (and observe what happens as time passes) -- explain everyting and nothing?
 
 - Useful to model the social world: population stats, group interactions, and complex (adaptive) social systems -- to understand how micro-level behavior leads to macro-level outcomes

   - When models do not have analytical solutions (complexity)
   - When macro outcome cannot be explained with the simple aggregation of micro (or individual) behavior (emergence and self-organization) -- inequality is perhaps one example
   - micro behavior can produce widly divergig macro outcomes (chaos)
   - no equilibrium or multiple equilibria and the choice of solution concept might be tricky (oscillation)

 - Elements -- the process can include some randomness

   - Agents (like small bots)
   - Decision rules (deterministic v probablistic -- some kind of randomness), which can be adaptive (e.g., learning, reproduction and movement)
   - Interaction structure (constraints) -- the rules and (preexisting) structure can be somewhere inter- or independent here
   - Environment, such as the landscape of resources

 - Randomness: Monte Carlo methods: Sample randomly, compute resul;ts, repeat and agreegate; noise/errors; probablistic decision-making (by agents)  

## Applications: Theory (toy models, small and simple) v empirical predictions (large and sophisticated)

 - Theory development

   - Shaw, Tsvetkova and Daneshvar (2011) -- macro outcomes from a set of empirically groudned behavioral assumptions (also to evade ethic concersn) -- e.g., takes four to spread the gossip and break the group apart
   - Segregation -- micro assumptions and mechanisms for observed social phenomenon -- sufficient but not necessary "explanations"
 
 - Empirical predictions: Pandemic, conflict (Cederman 2002 PNAS), traffic congestion management (in case of road closures) and evacuation of large venues
 
 - Relating ABMs to theory and data: Livet et al 2010 in the *Journal of Artificial Societies and Social Simulation*

## Examples

 - Coleman's boat (the formation of power hierarchy) -- see "Foundations of Social Theory" (1994)

 - Residential segregation (and mobility and movement) -- segregation is not aggregation of individual preferences as people are fine or tolerant

   - Schelling 1971 -- most people are tolerant until certain cutpoint in relation to the percent of out-group neighors and beyond that threshold they will move to a different area -- one HH's move will trigger the domino effect and lead to segregation -- cascading) -- assuming the cost of moving is uniform (and potentially low)

 - Cooperation -- evolutionary (which is not the same as "repeated") game theory (trial and error adaptation through learning and natural selection is a built process to shape our behavior -- rationality is not key but habits and instincts) -- mutation and replication (e.g., cooperation can lead to "successful" outcomes) both taking place -- computational game theory ("evolution of co-operation")

   - Axelrod 1984 -- tit-for-tat, imitating what opponent did in previous round is the winning strategy
   - Helbing and Yu 2009 -- tit-for-tat fails under noise -- imitation and migration may maintain cooperation regime despoite error -- noise is defined by not choosing the most optimal strategy due to randomness
   
 - Contagion -- contracting disease through fixed, static networks; health-related behavior; pro-social (such as generosity) or anti-social behavior (e.g., SI, SIS, SIR and SEIR)

   - Watts 1999 -- small-world phenomenon -- network clusters with fews ties among each other can help to contain disease

## Process

 - Input parameters -- using empirical data to calibrate the model when "setting up" parameters

 - Model -- using empirical data to "specify" the value of parameters (e.g., behavioral aspect of agents)

 - Output -- using empirical data to "validate" the output of the ABM exercise; create predicted data to design further (e.g., experimental) study to collecct more empirical data 

 - Reading: Mechanisms for the contagion of generosity

 - Reading: Schulz et al 2022: Network-based explanation on inequality perceptions

## NetLogo Tutorial

## Textbook

 - https://bookdown.org/amesoudi/ABMtutorial_bookdown/