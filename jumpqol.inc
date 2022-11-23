#if defined _jumpqol_included_
    #endinput
#endif
#define _jumpqol_included_

enum SettingType
{
    JUMPQOL_BOOL,
    JUMPQOL_INT,
    JUMPQOL_FLOAT,
};

enum SettingAllow
{
    JUMPQOL_ALLOW,
    JUMPQOL_STORE,
    JUMPQOL_BLOCK,
};

/**
 * Called when a setting is about to get changed.
 *
 * @param setting   Setting name.
 * @param client    Client index.
 * @param type      Setting type.
 * @param value_old The current value.
 * @param value_new The new value.
 * @return          JUMPQOL_ALLOW to allow the change, JUMPQOL_STORE to just store the value as the client's preference, JUMPQOL_BLOCK to change nothing.
 */
forward SettingAllow Jumpqol_OnSettingChange(const char[] setting, int client, SettingType type, any value_old, any value_new);

/**
 * Called after a setting has successfully been changed.
 *
 * @param setting   Setting name.
 * @param client    Client index.
 * @param type      Setting type.
 * @param value_old The current value.
 * @param value_new The new value.
 */
forward void Jumpqol_OnSettingChanged(const char[] setting, int client, SettingType type, any value_old, any value_new);

/**
 * Returns the number of settings available.
 *
 * @return          Count.
 */
native int Jumpqol_GetSettingCount();

/**
 * Get the setting name from its index.
 *
 * @param index     Setting index.
 * @param setting   Buffer for the setting name.
 * @param maxlength Length of the buffer.
 */
native void Jumpqol_GetSettingName(int index, char[] setting, int maxlength);

/**
 * Get the type of a setting.
 *
 * @param setting   Setting name.
 * @return          The setting type.
 */
native SettingType Jumpqol_GetSettingType(const char[] setting);

/**
 * Get the range of possible values for the setting.
 *
 * @param setting   Setting name.
 * @param range     The min and max values as floats.
 */
native void Jumpqol_GetSettingRange(const char[] setting, float range[2]);

/**
 * Set the value of a setting for a client.
 *
 * @param setting   Setting name.
 * @param client    Client index.
 * @param value     New value.
 * @param force     Whether the value should be applied without question (avoid OnSettingChange).
 * @return          True if the setting was successfully changed, false otherwise.
 */
native bool Jumpqol_SetSettingValue(const char[] setting, int client, any value, bool force=true);

/**
 * Get the value of a setting for a client.
 *
 * @param setting   Setting name.
 * @param client    Client index.
 * @return          The setting value.
 */
native any Jumpqol_GetSettingValue(const char[] setting, int client);

/**
 * Apply a prefence of a client.
 *
 * @param setting   Setting name.
 * @param client    Client index.
 * @param force     Whether the preference should be applied without question (avoid OnSettingChange).
 * @return          True if the setting was successfully changed, false otherwise.
 */
native bool Jumpqol_ApplyPreferredValue(const char[] setting, int client, bool force=true);

/**
 * Apply all the preferences of a client.
 *
 * @param client    Client index.
 * @param force     Whether the individual preferences should be applied without question (avoid OnSettingChange).
 * @return          True if all settings were successfully changed, false otherwise.
 */
native bool Jumpqol_ApplyPreferredValues(int client, bool force=true);