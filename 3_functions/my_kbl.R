# Styling for kableExtra'

pacman::p_load(
  knitr,
  kableExtra
)

options(
  # kable_styling_position = 'htb',
  # kable_styling_latex_options = c(
  #   'striped',
  #   'repeat_header'
  # ),
  # kable_styling_font_size = 9,
  knitr.table.toprule = '\\hline\\hline',
  knitr.table.bottomrule = '\\hline\\hline'
)

# Wrapper
my_kbl <- function(df, 
                   align = 'c',
                   format = 'latex',
                   font_size = 9,
                   caption,
                   label) {
  kbl(
    df, 
    format = format,
    booktabs = TRUE,
    align = align,
    caption = caption,
    label = label,
    linesep = '',
  ) %>% 
    kable_styling(
      font_size = font_size,
      latex_options = c(
        # 'striped', # Removing this for AEE formatting guidelines
        'hold_position',
        'repeat_header'
      )
    )
}