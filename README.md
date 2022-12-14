A Sourcemod plugin for TF2 jumping that adds various improvements through optional settings without affecting the core gameplay. Only works on Linux servers (for now) and requires a newish version of Sourcemod 1.11 (>=1.11.6820).

Players can use `/jumpqol` (`sm_jumpqol` in console) to see the available settings with descriptions. If a setting is more than just an on/off setting, then `/jumpqol <setting>` will show what the possible values that can be used are. Finally changing the value of a setting is done through `/jumpqol <setting> <value>`.

The server can similarly use `sm_jumpqol` to list all the settings along with the default values. The default values can be changed with the ConVars `sm_jumpqol_<setting>_default` and it is possible to enforce the default values by setting the ConVars `sm_jumpqol_<setting>_enforce 1`. A config file for these ConVars gets generated at `cfg/sourcemod/jumpqol.cfg`.

For finer control, the `jumpqol.inc` include lets other plugins control what setting values the players should have and informs plugins when they try to change them. An example of such a plugin can be seen in `example.sp`.
