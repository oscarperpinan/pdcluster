 # Copyright (C) 2011 Oscar Perpiñán Lamigueiro
 #
 # This program is free software; you can redistribute it and/or
 # modify it under the terms of the GNU General Public License
 # as published by the Free Software Foundation; either version 2
 # of the License, or (at your option) any later version.
 #
 # This program is distributed in the hope that it will be useful,
 # but WITHOUT ANY WARRANTY; without even the implied warranty of
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 # GNU General Public License for more details.
 #
 # You should have received a copy of the GNU General Public License
 # along with this program; if not, write to the Free Software
 # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 #/

setClass(
         Class='PD', 
         representation=representation(
           angle='numeric',             #vector of angles, degrees
           data='data.frame',           #data
           refl='logical',##vector
           filtered='logical',##after subset
           filter='call',##which filter I did?
           transformed='logical',##after transformPD
           refl.rm='logical',##!refl in filter?
           description='character'
           ),
         validity=function(object) {return(TRUE)}
         )


setClass(
         Class='PDCluster', 
         representation=representation(
           cluster='numeric',
           nSims='numeric',
           nClusters='numeric',
           metric='character',
           noise.level='numeric',
           noise.rm='logical',
           dist='data.frame' ##dist, distRel, distFactor
           ),
         contains='PD',
         validity=function(object) {return(TRUE)}
         )
