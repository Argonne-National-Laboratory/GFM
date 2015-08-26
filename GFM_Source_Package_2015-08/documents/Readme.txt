
!================================================================================
! Release notes for GFM 4.00.01
!================================================================================

August 2015

GFM is now being released with an open source license. Department of Energy funding for development ended before 2008. As noted in other documents the components have not been compiled with 64 bit compilers. The executibles in this download were compiled using an older computer.
 with the following compilers:

Compag Visual Fortran Professional Edition 6.6a

Microsoft Visual Basic 6.0 for 32-bit Windows Development

The combustion space code, which includes radiation heat transfer solvers, is in Fortran.

The melt space code is in Fortran.

The user interface, GUI, including grid construction, specification of boundary conditons and other parameters, run control, and some post processing visualization capabilities, is writen in Visual Basic. The GUI requires Visual Basic 6.0 runtime DLLs to function. Those are supplied in the package.

The installer scripts use the Nullsoft Scriptable Install System (NSIS) version 2.0.

The supplied files include the Fortran development environment and Visual Basic development environment files.

Compiling the supplied source code will likely require some modifications to achieve a successful compile and project build with currently available compilers.


February 2010

The difficulties of a trial user led to some minor changes in the melt models for side chargers (see notes below for cautions regarding side chargers and alternative approaches when side charger simulations do not converge). Some enhancements to charger configuration were added to the user interface software, and the following clarifications and advice for setting up melt simulations have been added to the Tips and Procedures document.

Melt Domain

•	Configuring chargers and setting batch/cullet velocity to match observed, expected, or desired batch/cullet coverage on the melt surface is crucial to achieving stable, physically reasonable results in the melter model. The batch and cullet transport and melting models in GFM are by necessity simplifications of very complex physical processes. The batch/cullet melting process is modeled as a process that takes place on and just above the surface of the melt. Batch and cullet enter the melt through chargers at a user specified velocity. This velocity is assumed to be the mean value that solids move across the melt surface away from a charger. At present, the models do not account for any spreading out of the batch or cullet on the surface as it moves away from a charger. In the hot furnace environment, the surface of the solids reaches the melting point very rapidly. This melting point is also user specified, and is usually much cooler, by several hundred degrees, than the melt surface that is not covered by batch or cullet. As a consequence, the bulk of the radiation energy that enters the melt space will be transferred to the surface of the solids. The heat flux pattern at the melt surface is, therefore, highly dependent on the batch and cullet coverage. The following steps are therefore recommended to obtain reasonably realistic melt surface conditions in the melt.

o	The charger width should be adjusted from its actual width to a width that yields a batch coverage on the melt surface that matches coverage observed, expected, or desired in the judgment of the furnace engineer.

o	Solids on the surface move away from a charger inlet in a straight line until heat transfer via radiation/convection from above and conduction from the melt below is sufficient to melt all the solids. If the solids are not completely melted before they reach the wall on the opposite side of the melter, the computation will not be numerically stable, and mass and energy balances will most likely not be achieved. This condition is not considered to be physically realistic or within the scope of problems that GFM can simulate in a steady state computation. In cases where solids do reach an opposite wall, the velocity of the solids specified by the user as the charger inlet velocity should be lowered until the batch coverage that is computed in the simulation is close to what is observed, expected, or desired.

o	Side chargers may yield numerically unstable results of failure to converge with melt mass balance if the batch/cullet does not melt before reaching the opposite side of the melter. For opposed side chargers the same difficulties arise if the batch/cullet does not melt completely before reaching the center of the melter. The batch/cullet model does not have a sub-model to handle the physics of cases where opposing batch/cullet flows on the surface collide and either spread out or move off in another direction. Currently, the best way to set up a simulation of a melter with side chargers is probably to move the chargers to the end on the long axis away from the exit and configure them to yield approximately the same batch/cullet surface coverage as is observed, expected, or desired from a side charger configuration.

o	Chargers must be positioned at the top of the melter. This position is required because the batch/cullet melting process is modeled as a surface process that takes place in and above the top layer of computational cells in the melt grid.

o	Note that charger configuration and setting of batch/cullet velocity may require changes and adjustments after an initial simulation yields an unexpected or undesired batch/cullet coverage on the melt surface or a coverage that does not result in complete melting.

o	The height of a charger just determines the height of the top layer of mesh cells in the melter computational grid. It does not need to correspond to the real charger height. Batch and cullet melting is modeled as a process that takes place in and above the top layer of melter computational cells. The height of the batch and cullet above the surface does not need to be accounted for by the user in defining charger height. The software will feed in batch and cullet at the rate specified by the user even though the specified charger height does not appear large enough to accommodate the charging rate. This is because the top of the charger is really open for the purposes of the simulation and feeding solids into the melt. Entering a large charger height may cause it to be truncated to the maximum height allowed for a grid cell, which depends on the melter dimensions and coarseness. This is not a problem. A charger spanning two or more grid cells in height, however, is a problem because the melting model for batch and cullet is active only in the top layer of grid cells. If the user modifies the melter dimensions, changes grid density, or add/deletes grid lines, then the user should make sure the charger is only one cell high.

o	To achieve mass balance in the melt (solids in = molten glass out), the force of gravity must be able to act to drain the melter. For this reason, the bottom of a charger should not be below the top of an outlet. This means that the “bottom gap” of a charger must be greater than or equal to the “bottom gap” plus the “height” of all outlets. In general, numerical stability and convergence with mass balance are enhanced when outlets are at larger depths below the melt surface because the static head due to gravitational force acting to move molten glass out the exit is greater.

•	Refiners may lead to numerical instability and failure to converge for a number of reasons. Among them flow through the throat and in the refiner are difficult to resolve in the relatively coarse grids required to run on a single computer, there is some heat loss through the wall of a refiner, but currently no provision to maintain temperature in a refiner via small burners above the surface, and refiner outlets may be very close to the melt surface giving very little static head force to move molten glass out the exits. If simulation with a refiner does not converge well enough to give useful results for the overall furnace analysis, building the melter without a refiner and letting the melter throat be the melter outlet may provide converged results that are good enough for evaluating overall furnace efficiency and achieving the other goals of the analysis.



!================================================================================
! Release notes for GFM 4.00.00
!================================================================================

Release 4 contains major upgrades to most of the models and algorithms in both
the combustion space and melt space components. The upgrades were implimented to
improve the convergence levels that can be achieved in melt space and combustion
space computations in order to make it possible to couple solution in the two
and achieve stable convergence in a computation that cycles between the melt
and combustion space exchanging values of the coupling variables, temperature
and heat flux, at the interface.

Release 3 has been skipped because initial planning for release 3 was changed 
dramatically in the course of development of release 4.

Details of these upgrades and new setup and procedures for model calibration,
automatic cycling runs, etc. are provided in new documentation. A good overview
with more detailed information on GFM 4.0 is in the final report for Version 4.0, which is included in the documentation. The installer puts the documentation in the
folder where GFM is installed. The file GFM4-Using-GUI.pdf contains two examples of
how to build and run a furnace model using the graphical user interface and control program. The GFM4-Tips-Procedures.pdf document provides useful pointers, the soot
model calibration procedure, and a procedure for adjusting relaxation factors and
other parameters for models that do not converge well.

Please refer any questions to: 

Steven A. Lottes, Ph.D.
Energy Systems Division
Argonne National Laboratory

SLottes@ANL.gov

!================================================================================
! Release notes for GFM 2.07.01
!================================================================================

Contains a fix to handling radiation in the combustion space.
Also adds menu items to bring up or switch to various parameter or
property boxes directly, and related changes.

!================================================================================
! Release notes for GFM 2.07.00
!================================================================================

This is the first major release including significant upgrades and a large number
of bug fixes in the melt and combustion codes, since GFM maintenance and 
development were taken over by S.A. Lottes.

The release is still considered a beta version, please report any problems.

There are numerous upgrades and revisions in the GUI. Most of these are
described below under the notes for 2.06.00. Also see the document
GFM-WorkFlow.pdf in the GFM installation directory for a 
description of how to set up runs using cases in the new GUI version.

A major new capability allows the user to set up automatic cycling between
the combustion space and the melt space computations. During automatic
cycling, the melt surface condition information is automatically exchanged 
between the spaces so that a run coupling combustion and melt space can proceed
through a user specified number of cycles unattended. Notes on setting up automatic
cycling runs are contained in the document CycleDescription.pdf.

Other documentation and notes are under preparation. Please refer any questions
to: 

!================================================================================
! Release notes for GFM 2.06.00
!================================================================================

This release of GFM 2.06.00 is what probably should be called a pre-beta release.

Feedback on any problems uncovered would be greatly appreciated.

The GUI has been extensively reworked. Much of this effort was to eliminate a
variety of ways in which the user could cause the GUI to hang up, terminate
abnormally, or do other things that it should not do as a consequence of the 
user clicking things that were not part of the expected working procedures.

A fair amount of effort also went into making the GUI easier to use.

The two most significant changes are:

1. The GUI now allows the user to go back and forth between the screen with
the figure (schematic) and the screen with the grid via a "view" menu. 

In addition to convenience (the grid can be regenerated and viewed as each 
burner, exhaust, etc. is added) this feature enables a capability that
did not exist before. If a user made hand edits to the grid in
the grid view, saved the grid, and ran a case, previously there was no 
way to start the program up again and make changes to flow rates, inlet 
temperatures, or other conditions and run the new case without regenerating
the grid, which would destroy any user edits made to the grid. 

In GFM 2.06.00 the user can make hand edits to a grid, at which point the
grid will be "locked" with respect to the ability to add or modify objects
such as burners in the figure view. However, all the list boxes that provide
access to the various run parameters in "figure view" are accessible, and 
any parameter that does not require a regeneration of the grid can be modified 
and the new case can be run without destroying the user's hand edits to the grid.

A menu option also exists that allows the user to unlock a grid with hand edits 
in it in order to add or move burners, etc. to create a new case from an 
existing case. However, adding or moving one of these grid objects will require
the grid to be regenerated and any user hand edits will have to be reapplied.

2. The GUI now deals with "cases" and does nearly all the file management for the user.
The user need not know the information that is stored in individual files or when 
they are required to be loaded or saved within the GUI. Working with cases is very
simple. The file menu allows the user to create a new case, delete a case, save a case,
save a case as a new case (new case number), and open a case to continue work on it. 

User documentation for these revisions is not yet implimented in the help system, 
and it has not yet been incorporated into the user manual. A brief description of
how to use the new version of the GUI is included in the file GFM-WorkFlow.pdf,
which can be found in the directory where GFM is installed.

The uninstall program deletes GFM information from the registry and the GFM 
executable files, but it will not delete user generated case files.



