# Simulation d'une population avec prévalence
# Le premier argument (data) correspond aux données de recensement Insee, les autres aux paramètres par strate
sim_pop <- function(
  data, # recensement Insee (doit êttre préalablement importé),
  r0 = -1.8, r_age40 = -.25, r_age60 = -.8, r_age80 = -1.2, r_sexe = -.1,
  r_csp2 = 0, r_csp3 = 0, r_csp4 = -.1, r_csp5 = -.5, r_csp6 = .3, r_csp7 = -.4, r_csp8 = 0 
) {
  data %>% 
    mutate(
      prev = plogis(r0 + 
                      r_age40 * (age >= 40 & age < 60) +
                      r_age60 * (age >= 60 & age < 80) +
                      r_age80 * (age >= 80) +
                      r_csp2 * (csp == 2) + 
                      r_csp3 * (csp == 3) + 
                      r_csp4 * (csp == 4) + 
                      r_csp5 * (csp == 5) + 
                      r_csp6 * (csp == 6) + 
                      r_csp7 * (csp == 7) + 
                      r_csp8 * (csp == 8)
      ),
      prev = rnorm(nrow(.), prev, prev * (1 - prev) / 2),
      prev = pmax(prev, 0)
    )
}


# Échantillonnage à partir d'une population simulée
# Le premier argument (data) correspond à la population simulée avec sim_pop
draw_sample <- function(data, prob_max, cohort_noise = .2, strata_noise = .1,
                        nA = 2686, nB = 2934, nC = 112, nD = 119, nE = 497) {
  cohort_lag <- runif(5, 1 - cohort_noise, 1 + cohort_noise)
  bind_rows(
    data %>% 
      filter(age >= 20, age <= 80) %>%
      mutate(w0 = pond_pop / nA * cohort_lag[1]) %>% 
      sample_n(size = nA, weight = w0, replace = TRUE) %>% 
      mutate(w0 = runif(nA, w0 * (1 - strata_noise), w0 * (1 + strata_noise))) %>% 
      mutate(cohorte = "A"),
    
    data %>% 
      filter(age >= 20, age <= 90) %>%
      mutate(w0 = pond_pop / nB * cohort_lag[2]) %>% 
      sample_n(size = nB, weight = w0, replace = TRUE) %>% 
      mutate(w0 = runif(nB, w0 * (1 - strata_noise), w0 * (1 + strata_noise))) %>% 
      mutate(cohorte = "B"),
    
    data %>% 
      filter(age >= 70, age <= 90, sexe_m == 0) %>%
      mutate(w0 = pond_pop / nC * cohort_lag[2]) %>% 
      sample_n(size = nC, weight = w0, replace = TRUE) %>% 
      mutate(w0 = runif(nC, w0 * (1 - strata_noise), w0 * (1 + strata_noise))) %>% 
      mutate(cohorte = "C"),
    
    data %>% 
      filter(age >= 70, age <= 90, sexe_m == 1) %>%
      mutate(w0 = pond_pop / nD * cohort_lag[4]) %>% 
      sample_n(size = nD, weight = w0, replace = TRUE) %>% 
      mutate(w0 = runif(nD, w0 * (1 - strata_noise), w0 * (1 + strata_noise))) %>% 
      mutate(cohorte = "D"),
    
    data %>% 
      filter(age >= 25, age <= 70) %>%
      mutate(w0 = pond_pop / nE * cohort_lag[5]) %>% 
      sample_n(size = nE, weight = w0, replace = TRUE) %>% 
      mutate(w0 = runif(nE, w0 * (1 - strata_noise), w0 * (1 + strata_noise))) %>% 
      mutate(cohorte = "E")
    
  ) %>% 
    mutate(statut = rbinom(nrow(.), 1, prev)) %>% 
    select(cohorte, everything(), poids_sond = w0, -pond_pop, -prev)
}


# Masquage aléatoire des poids de sondage
# Le premier argument (data) correspond à un échantillon obtenu avec draw_sample
hide_weights <- function(
  data, p0 = -5, p_age = .02, p_sexe = -.2, 
  p_csp2 = .04, p_csp3 = -.03, p_csp4 = -.02, p_csp5 = -.04, p_csp6 = -.03, p_csp7 = -.06, p_csp8 = -.05) {
  data %>% 
    mutate(
      p_hdn = plogis(p0 + p_age * age + p_sexe * sexe_m +
        p_csp2 * (csp == 2) + 
        p_csp3 * (csp == 3) + 
        p_csp4 * (csp == 4) + 
        p_csp5 * (csp == 5) + 
        p_csp6 * (csp == 6) + 
        p_csp7 * (csp == 7) + 
        p_csp8 * (csp == 8)),
      hdn = rbinom(nrow(.), 1, p_hdn),
      poids_sond = ifelse(hdn == 1, NA, poids_sond)
    ) %>% 
    select(-p_hdn, hdn)
}
