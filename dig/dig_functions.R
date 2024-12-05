# functions for generating some nice output for the DIG
# requires library(flextable)
# requires library(here)
# requires library(officer)
mef_stage <- c("Diagnose","Plan","Measure","Reflect")
mef_stage_url <- c(
  "https://taso.org.uk/step-1-diagnose/",
  "https://taso.org.uk/step-2-plan/",
  "https://taso.org.uk/step-3-measure/",
  "https://taso.org.uk/step-4-reflect/")
mef_image <- here::here("images/dig-images/eval_cycle_icon_small.png")
audience_image <- here::here("images/dig-images/im_audience.png")
table_border <- officer::fp_border(color = "lightgrey", style = "solid", width = 1)

# generate a table for the MEF stage
# 1. Diagnose
# 2. Plan
# 3. Measure
# 4. Reflect
genMEFTable <- function(stage = 1) {
  # data frame containing MEF image filename, stage and url
  .df <- data.frame(image=mef_image, stage=mef_stage[stage], url = mef_stage_url[stage])
  flextable::flextable(.df) |> 
    flextable::delete_rows(1,part="header") |>
    #delete display of column 3 (url) from the table
    flextable::delete_columns(3) |>
    # take data from the image column of .df and load the filename as an image
    flextable::compose(i=1,
                       j=1, 
                       value = flextable::as_paragraph(
                         flextable::as_image(image, 
                                        unit="mm",
                                        width=15, 
                                        height=15)),
                       part="body") |>
    # take data from the url column of .df and apply it as a hyperlink to stage column
    flextable::compose(i=1,
                       j=2,
                       value = flextable::as_paragraph(
                         flextable::hyperlink_text(x = stage,
                                        url = url,
                                        props=officer::fp_text(color="#3b66bc",
                                                               underlined = TRUE,
                                                               font.family = "Source Sans Pro"))
                       )) |>
    flextable::border_outer(border = table_border) |>
    flextable::border_inner(border = table_border) |>
    flextable::width(j=2, width = 50, unit="mm") |>
    flextable::font(part = "all",fontname = "Source Sans Pro") |>
    flextable::align(j=1,align="center")
}

# Generate a table for the audience only
genAudienceTable <- function(audience = c("")) {
  # sort the audience alphabetically
  .audience <- paste(sort(audience), collapse="\n")
  .df <- data.frame(image=audience_image, .audience)
  flextable::flextable(.df) |> 
    flextable::font(part = "all",fontname = "Source Sans Pro") |>
    flextable::delete_rows(1,part="header") |> 
    # take data from the image column of .df and load the filename as an image
    flextable::compose(i=1,
                       j=1, 
                       value = flextable::as_paragraph(
                         flextable::as_image(audience_image, 
                                  unit="mm",
                                  width=15, 
                                  height=15)),
                       part="body") |>
    flextable::border_outer(border = table_border) |>
    flextable::border_inner(border = table_border) |>
    flextable::width(j=2, width = 50, unit="mm") |>
    flextable::align(j=1,align="center")
}

# Generate a table for the MEF stage and the audience
genMEFAudienceTable <- function(stage = 1, audience = c("")) {
  # sort the audience alphabetically
  .audience <- paste(sort(audience), collapse="\n")
  
  .df <- data.frame(image=c(mef_image, audience_image),
                    data=c(mef_stage[stage],.audience),
                    url=c(mef_stage_url[stage],""))
  flextable::flextable(.df) |> 
    flextable::delete_rows(1,part="header") |>
    #delete display of column 3 (url) from the table
    flextable::delete_columns(3) |>
    # take data from the image column of .df and load the filename as an image
    flextable::compose(j=1, 
                       value = flextable::as_paragraph(
                         flextable::as_image(image, 
                                  unit="mm",
                                  width=15, 
                                  height=15)),
                       part="body") |>
    # take data from the url column of .df and apply it as a hyperlink to data column of .df
    flextable::compose(i=1,
                       j=2,
                       value = flextable::as_paragraph(
                         flextable::hyperlink_text(x = data, 
                                        url = url, 
                                        props=officer::fp_text(color="#3b66bc", 
                                                               underlined = TRUE,
                                                               font.family = "Source Sans Pro"))
                       )) |>
    flextable::border_outer(border = table_border) |>
    flextable::border_inner(border = table_border) |>
    flextable::width(j=2, width = 50, unit="mm") |>
    flextable::font(part = "all",fontname = "Source Sans Pro") |>
    flextable::align(j=1,align="center")
}

# Generate key for barriers/facilitators
genKeyBarriersFacilitators <- function() {
  .text = c("Ethics/Privacy",
            "Organisational",
            "Data infrastructure",
            "Data")
  .images = c(here::here("images/dig-images/im_privacyethics.png"),
              here::here("images/dig-images/im_organisation.png"),
              here::here("images/dig-images/im_infrastructure.png"),
              here::here("images/dig-images/im_data.png"))
  
  .df <- data.frame(image=.images, text=.text)
  flextable::flextable(.df) |> 
    flextable::font(part = "all",fontname = "Source Sans Pro") |>
    flextable::delete_rows(1,part="header") |>
    flextable::compose(j=1,
                       value = flextable::as_paragraph(
                         flextable::as_image(image, 
                                             unit="mm",
                                             width=15, 
                                             height=15)),
                       part="body") |> 
    flextable::border_outer(border = table_border) |>
    flextable::border_inner(border = table_border) |>
    flextable::width(j=2, width = 50, unit="mm") |>
    flextable::align(j=1,align="center")
}
