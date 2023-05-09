package Web

import cask.Redirect
import Data.{AccountService, SessionService, Session}
import Web.Decorators.getSession

/**
  * Assembles the routes dealing with the users:
  * - One route to display the login form and register form page
  * - One route to process the login form and display the login success page
  * - One route to process the register form and display the register success page
  * - One route to logout and display the logout success page
  * 
  * The username of the current session user is stored inside a cookie called `username`.
  */
class UsersRoutes(accountSvc: AccountService,
                  sessionSvc: SessionService)(implicit val log: cask.Logger) extends cask.Routes:
    // TODO - Part 3 Step 3a: Display a login form and register form page for the following URL: `/login`.
    @getSession(sessionSvc)
    @cask.get("/login")
    def login()(session : Session) =
        log.debug("GET /login")
        session.getCurrentUser match
            case Some(user) => Layouts.index(Option(user))
            case None => Layouts.login()
    

    // DONE - Part 3 Step 3b: Process the login information sent by the form with POST to `/login`,
    //      set the user in the provided session (if the user exists) and display a successful or
    //      failed login page.
    @getSession(sessionSvc)
    @cask.postForm("/login")
    def login_post(username: cask.FormValue)(session : Session)= 
        log.debug(s" Auth with username: ${username.value} ")
        accountSvc.isAccountExisting(username.value) match
            case false =>{        
                log.debug(s" User ${username.value} doesn't exist")
                Layouts.login(login_failed = true)
            }
            case true => {
                session.setCurrentUser(username.value)
                Layouts.login(flash = Some(s"Login avec succès!"))
            }
    end login_post

    //
    // DONE - Part 3 Step 3c: Process the register information sent by the form with POST to `/register`,
    //      create the user, set the user in the provided session and display a successful
    //      register page.
    //
    @getSession(sessionSvc) 
    @cask.postForm("/register")
    def register(username:cask.FormValue)(session : Session) =
        accountSvc.isAccountExisting(username.value) match
            case true => {
                log.debug(s" User ${username.value} already exists")
                Layouts.login(register_failed = true)
            }
            case false => {
                log.debug(s" User ${username.value} created")
                accountSvc.addAccount(username.value,30.0)
                session.setCurrentUser(username.value)
                cask.Redirect("/login")
                Layouts.login(flash = Some(s"Enregistrement avec succès"))
            }
    // DONE - Part 3 Step 3d: Reset the current session and display a successful logout page.
    @getSession(sessionSvc)
    @cask.get("/logout")
    def logout()(session : Session) =
        log.debug("GET /logout")
        session.reset()
        Layouts.index(None)
    initialize()
end UsersRoutes
