setMethod('show', 'PD',
          function(object){
            cat('Object of class ', class(object),'\n\n')
            cat('Source of measurements: ')
            cat(object@description,'\n')
            cat('Number of observations: ', nrow(object@data), '\n')
            cat('Filtered?: ', isFiltered(object), '\n')
            cat('Filter: ')
            print(object@filter)
            cat('Transformed?:', isTransformed(object),'\n')
            cat('\nData:\n')
            print(summary(object@data))
            cat('Number of reflections: ', sum(object@refl),'\n')
          }
          )

setMethod('show', 'PDCluster',
          function(object){
            show(as(object, 'PD'))
            cat('Number of clusters: ', object@nClusters, '\n')
            cat('Number of elements per cluster:\n')            
            print(table(object@cluster))
            cat('\nMetric: ', object@metric, '\n')
            cat('Number of simulations: ', object@nSims, '\n')
            if (object@noise.rm) cat('Noise level: ', object@noise.level, '\n')
            cat('Distances', '\n')
            print(summary(object@dist))            
          }
          )
