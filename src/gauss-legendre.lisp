(in-package :funcl)

;; Numerical integration with an implicit fourth order Gauss-Legendre Runge-Kutta method via GSL.

(defun integrate-ode-system-gsl (initial-state f jacobian &optional (solver gsl:+step-rk4imp+))
  "Integrates an ODE system using any of GSL's provided solvers. 
   jacobian is required for the implicit solvers and returns (values df_i/dt  df_i/df_j), the partial derivatives with respect to time are required as well."
  )
