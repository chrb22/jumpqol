#include <jumpqol.inc>
#include <tf2.inc>

public void OnPluginStart()
{
	int count = Jumpqol_GetSettingCount();
	for (int setting = 0; setting < count; setting++) {
		char name[64];
		Jumpqol_GetSettingName(setting, name, 64);
		PrintToServer("setting: %s", name);
	}

	HookEvent("player_death", OnClientDeath);
}

public SettingAllow Jumpqol_OnSettingChange(const char[] setting, int client, SettingType type, any value_old, any value_new)
{
	PrintToServer("setting about to change: %s", setting);

	if (StrEqual(setting, "sync") && view_as<TFTeam>(GetClientTeam(client)) != TFTeam_Blue) {
		ReplyToCommand(client, "need to be on blu team to change your sync setting");
		return JUMPQOL_BLOCK;
	}

	if (StrEqual(setting, "fakedelay")) {
		ReplyToCommand(client, "respawn for fakedely to apply");
		return JUMPQOL_STORE;
	}

	return JUMPQOL_ALLOW;
}

void OnClientDeath(Event event, const char[] name, bool dontBroadcast)
{
    int client = GetClientOfUserId(GetEventInt(event, "userid"));

    Jumpqol_ApplyPreferredValue("fakedelay", client);
}

public void Jumpqol_OnSettingChanged(const char[] setting, int client, SettingType type, any value_old, any value_new)
{
	PrintToServer("setting changed: %s", setting);
}