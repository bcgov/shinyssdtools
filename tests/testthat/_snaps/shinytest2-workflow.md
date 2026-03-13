# workflow: data -> fit, plot and table render, state values

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["input"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["fit_mod-rescale", "fit_mod-selectConc", "fit_mod-selectDist", "fit_mod-selectUnit"]
            }
          },
          "value": [
            {
              "type": "logical",
              "attributes": {},
              "value": [false]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Conc"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["gamma", "lgumbel", "llogis", "lnorm", "lnorm_lnorm", "weibull"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": [""]
            }
          ]
        }
      ]
    }

# workflow: data -> predict, plot and table render, state values

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["input"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["predict_mod-bootSamp", "predict_mod-includeCi", "predict_mod-thresh", "predict_mod-threshType"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["1,000"]
            },
            {
              "type": "logical",
              "attributes": {},
              "value": [true]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["5"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Concentration"]
            }
          ]
        }
      ]
    }

