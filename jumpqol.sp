#pragma semicolon 1
#pragma newdecls required

#include <sourcemod>
#include <dhooks>
#undef REQUIRE_EXTENSIONS
#include <sendvaredit>
#define REQUIRE_EXTENSIONS

public Plugin myinfo =
{
    name = "JumpQoL",
    author = "ILDPRUT",
    description = "Adds various improvements to jumping.",
    version = "1.1.2",
}

#define DEBUG 0





/*
_struct
███████ ████████ ██████  ██    ██  ██████ ████████
██         ██    ██   ██ ██    ██ ██         ██   
███████    ██    ██████  ██    ██ ██         ██   
     ██    ██    ██   ██ ██    ██ ██         ██   
███████    ██    ██   ██  ██████   ██████    ██   
*/





methodmap Globals
{
    public Globals(Address address)
    {
        return view_as<Globals>(address);
    }

    property float curtime
    {
        public get()
        {
            return view_as<float>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(12), NumberType_Int32) );
        }
        public set(float curtime)
        {
            StoreToAddress(view_as<Address>(this) + view_as<Address>(12), view_as<int>(curtime), NumberType_Int32, false);
        }
    }

    property float frametime
    {
        public get()
        {
            return view_as<float>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(16), NumberType_Int32) );
        }
        public set(float frametime)
        {
            StoreToAddress(view_as<Address>(this) + view_as<Address>(16), view_as<int>(frametime), NumberType_Int32, false);
        }
    }

    property int tickcount
    {
        public get()
        {
            return view_as<int>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(24), NumberType_Int32) );
        }
        public set(int tickcount)
        {
            StoreToAddress(view_as<Address>(this) + view_as<Address>(24), view_as<int>(tickcount), NumberType_Int32, false);
        }
    }

    property float interval_per_tick
    {
        public get()
        {
            return view_as<float>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(28), NumberType_Int32) );
        }
    }
};





methodmap Vector
{
    public Vector(Address address)
    {
        return view_as<Vector>(address);
    }

    property float x
    {
        public get()
        {
            return view_as<float>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(0), NumberType_Int32) );
        }
        public set(float x)
        {
            StoreToAddress(view_as<Address>(this) + view_as<Address>(0), view_as<int>(x), NumberType_Int32, false);
        }
    }

    property float y
    {
        public get()
        {
            return view_as<float>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(4), NumberType_Int32) );
        }
        public set(float y)
        {
            StoreToAddress(view_as<Address>(this) + view_as<Address>(4), view_as<int>(y), NumberType_Int32, false);
        }
    }

    property float z
    {
        public get()
        {
            return view_as<float>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(8), NumberType_Int32) );
        }
        public set(float z)
        {
            StoreToAddress(view_as<Address>(this) + view_as<Address>(8), view_as<int>(z), NumberType_Int32, false);
        }
    }
};

methodmap Plane
{
    public Plane(Address address)
    {
        return view_as<Plane>(address);
    }

    property Vector normal
    {
        public get()
        {
            return view_as<Vector>( view_as<Address>(this) + view_as<Address>(0) );
        }
    }

    property float dist
    {
        public get()
        {
            return view_as<float>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(12), NumberType_Int32) );
        }
        public set(float dist)
        {
            StoreToAddress(view_as<Address>(this) + view_as<Address>(12), view_as<int>(dist), NumberType_Int32, false);
        }
    }

    property int type
    {
        public get()
        {
            return view_as<int>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(16), NumberType_Int8) );
        }
        public set(int type)
        {
            StoreToAddress(view_as<Address>(this) + view_as<Address>(16), view_as<int>(type), NumberType_Int8, false);
        }
    }

    property int signbits
    {
        public get()
        {
            return view_as<int>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(17), NumberType_Int8) );
        }
        public set(int signbits)
        {
            StoreToAddress(view_as<Address>(this) + view_as<Address>(17), view_as<int>(signbits), NumberType_Int8, false);
        }
    }
};

methodmap Trace
{
    public Trace(Address address)
    {
        return view_as<Trace>(address);
    }

    property Vector startpos
    {
        public get()
        {
            return view_as<Vector>( view_as<Address>(this) + view_as<Address>(0) );
        }
    }

    property Vector endpos
    {
        public get()
        {
            return view_as<Vector>( view_as<Address>(this) + view_as<Address>(12) );
        }
    }

    property Plane plane
    {
        public get()
        {
            return view_as<Plane>( view_as<Address>(this) + view_as<Address>(24) );
        }
    }
};





methodmap SendProp
{
    public SendProp(Address address)
    {
        return view_as<SendProp>(address);
    }

    property int type
    {
        public get()
        {
            return view_as<int>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(8), NumberType_Int32) );
        }
    }

    property int numbits
    {
        public get()
        {
            return view_as<int>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(12), NumberType_Int32) );
        }
    }

    property float lowvalue
    {
        public get()
        {
            return view_as<float>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(16), NumberType_Int32) );
        }
    }

    property float highvalue
    {
        public get()
        {
            return view_as<float>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(20), NumberType_Int32) );
        }
    }

    property Address name
    {
        public get()
        {
            return view_as<Address>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(48), NumberType_Int32) );
        }
    }

    property float highlowmul
    {
        public get()
        {
            return view_as<float>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(52), NumberType_Int32) );
        }
    }

    property int flags
    {
        public get()
        {
            return view_as<int>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(56), NumberType_Int32) );
        }
    }

    property int offset
    {
        public get()
        {
            return view_as<int>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(72), NumberType_Int32) );
        }
    }
};





methodmap IPredictionSystem
{
    public IPredictionSystem(Address address)
    {
        return view_as<IPredictionSystem>(address);
    }

    property Address m_pSuppressHost
    {
        public get()
        {
            return view_as<Address>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(12), NumberType_Int32) );
        }

        public set(Address m_pSuppressHost)
        {
            StoreToAddress(view_as<Address>(this) + view_as<Address>(12), view_as<int>(m_pSuppressHost), NumberType_Int32, false);
        }
    }
};





/*
_util
██    ██ ████████ ██ ██     
██    ██    ██    ██ ██     
██    ██    ██    ██ ██     
██    ██    ██    ██ ██     
 ██████     ██    ██ ███████
*/





#define NaN                                     view_as<float>(0x7FFFFFFF)                              // NaN
static const float NaNVector[3] =               {NaN, NaN, NaN};
#define Inf                                     view_as<float>(0x7F800000)                              // Inf
#define DIST_EPSILON                            (0.03125)

bool FloatIsNaN(float f) { return f != f; }
// float FloatMin(float f1, float f2) { return (f1 < f2) ? f1 : f2; }
float FloatMax(float f1, float f2) { return (f1 > f2) ? f1 : f2; }
float FloatClamp(float f, float min, float max) { return f < min ? min : (f > max ? max : f); }

bool VectorIsNaN(const float vec[3])
{
    for (int i = 0; i < 3; i++)
        if (FloatIsNaN(vec[i]))
            return true;

    return false;
}

void CopyVector(const float vec[3], float[] buffer)
{
    for (int i = 0; i < 3; i++)
        buffer[i] = vec[i];
}

float DotVectors(const float vec1[3], const float vec2[3])
{
    float dot = 0.0;
    for (int i = 0; i < 3; i++)
        dot += vec1[i]*vec2[i];
    return dot;
}

void OffsetVector(const float vec[3], const float dir[3], float scale, float buffer[3])
{
    for (int i = 0; i < 3; i++)
        buffer[i] = vec[i] + dir[i]*scale;
}

bool CompareVectors(const float[] vec1, const float[] vec2)
{
    for (int i = 0; i < 3; i++)
        if (FloatAbs(vec1[i] - vec2[i]) >= 0.001)
            return false;
    return true;
}





bool IsActivePlayer(int entity)
{
    return 1 <= entity <= MaxClients && IsClientInGame(entity) && !IsFakeClient(entity) && IsPlayerAlive(entity);
}





float[] LoadFromVectorAddress(any vector)
{
    float array[3];
    array[0] = LoadFromAddress(view_as<Address>(vector) + view_as<Address>(0*4), NumberType_Int32);
    array[1] = LoadFromAddress(view_as<Address>(vector) + view_as<Address>(1*4), NumberType_Int32);
    array[2] = LoadFromAddress(view_as<Address>(vector) + view_as<Address>(2*4), NumberType_Int32);
    return array;
}

void StoreToVectorAddress(const float array[3], any vector)
{
    StoreToAddress(view_as<Address>(vector) + view_as<Address>(0*4), array[0], NumberType_Int32, false);
    StoreToAddress(view_as<Address>(vector) + view_as<Address>(1*4), array[1], NumberType_Int32, false);
    StoreToAddress(view_as<Address>(vector) + view_as<Address>(2*4), array[2], NumberType_Int32, false);
}

int LoadFromStringAddress(Address cstring, char[] string)
{
    int i = 0;
    do {
        string[i] = LoadFromAddress(cstring + view_as<Address>(i), NumberType_Int8);
    }
    while (string[i++] != 0);

    return i - 1;
}





ConVar g_ConVar_sv_gravity = view_as<ConVar>(INVALID_HANDLE);
float GetGravity(int client)
{
    if (!g_ConVar_sv_gravity)
        g_ConVar_sv_gravity = FindConVar("sv_gravity");

    int g_m_flGravity_offset = FindDataMapInfo(client, "m_flFriction") - 4; // m_flGravity is right before m_flFriction
    float m_flGravity = GetEntDataFloat(client, g_m_flGravity_offset);
    if (m_flGravity < 0.000001) // Not sure why this is a thing
        m_flGravity = 1.0;

    return m_flGravity * GetConVarFloat(g_ConVar_sv_gravity);
}





/*
_global
 ██████  ██       ██████  ██████   █████  ██     
██       ██      ██    ██ ██   ██ ██   ██ ██     
██   ███ ██      ██    ██ ██████  ███████ ██     
██    ██ ██      ██    ██ ██   ██ ██   ██ ██     
 ██████  ███████  ██████  ██████  ██   ██ ███████
*/





char g_error[256] = "";
bool SetError(const char[] error)
{
    strcopy(g_error, sizeof(g_error), error);
    return false;
}





Handle g_gameconf;





bool g_sendvaredit_loaded = false;





Globals g_globals;

float g_curtime_frame;
float g_frametime_frame;
int g_tickcount_frame;





int g_entity_simulating = -1;
int g_player_simulating = -1;
int g_weapon_simulating = -1;

bool g_inside_cmd = false;

bool g_allow_update = false;





/*
_detour
██████  ███████ ████████  ██████  ██    ██ ██████ 
██   ██ ██         ██    ██    ██ ██    ██ ██   ██
██   ██ █████      ██    ██    ██ ██    ██ ██████ 
██   ██ ██         ██    ██    ██ ██    ██ ██   ██
██████  ███████    ██     ██████   ██████  ██   ██
*/





enum struct Detour
{
    Handle detour;
    char name[64];

    void Init(const char[] name, CallingConvention callconv, ReturnType returntype, ThisPointerType thistype, const HookParamType[] params)
    {
        strcopy(this.name, sizeof(this.name), name);

        this.detour = DHookCreateDetour(Address_Null, callconv, returntype, thistype);
        if (!DHookSetFromConf(this.detour, g_gameconf, SDKConf_Signature, this.name)) {
            this.detour = INVALID_HANDLE;
            return;
        }

        int i = 0;
        while (params[i] != HookParamType_Unknown) {
            DHookAddParam(this.detour, params[i]);
            i++;
        }
    }

    bool Enable(DHookCallback pre = INVALID_FUNCTION, DHookCallback post = INVALID_FUNCTION)
    {
        if (this.detour == INVALID_HANDLE) {
            Format(g_error, sizeof(g_error), "Failed to initialize detour %s.", this.name);
            return false;
        }

        bool fail = false;

        if (pre != INVALID_FUNCTION && !DHookEnableDetour(this.detour, false, pre)) {
            Format(g_error, sizeof(g_error), "Failed to enable detour %s (pre).", this.name);
            fail = true;
        }

        if (post != INVALID_FUNCTION && !DHookEnableDetour(this.detour, true, post)) {
            Format(g_error, sizeof(g_error), "Failed to enable detour %s (post).", this.name);
            fail = true;
        }

        if (fail) {
            this.Disable(pre, post);
            return false;
        }

        return true;
    }

    void Disable(DHookCallback pre = INVALID_FUNCTION, DHookCallback post = INVALID_FUNCTION)
    {
        if (pre != INVALID_FUNCTION)
            DHookDisableDetour(this.detour, false, pre);

        if (post != INVALID_FUNCTION)
            DHookDisableDetour(this.detour, true, post);
    }
}

enum
{
    DETOUR_PHYSICS_SIMULATEENTITY,
    DETOUR_SERVICEEVENTS,
    DETOUR_ITEMPOSTFRAME,
    DETOUR_UTIL_DECALTRACE,
    DETOUR_TFPLAYERTHINK,
    DETOUR_ITEMBUSYFRAME,
    DETOUR_CLIENT_ITEMPOSTFRAME,
    DETOUR_CLIENT_ITEMBUSYFRAME,
    DETOUR_SETGROUNDENTITY,
    DETOUR_CM_CLIPBOXTOBRUSH,
    NUM_DETOURS,
};
Detour g_detours[NUM_DETOURS];





/*
_setting
███████ ███████ ████████ ████████ ██ ███    ██  ██████ 
██      ██         ██       ██    ██ ████   ██ ██      
███████ █████      ██       ██    ██ ██ ██  ██ ██   ███
     ██ ██         ██       ██    ██ ██  ██ ██ ██    ██
███████ ███████    ██       ██    ██ ██   ████  ██████ 
*/





enum SettingType
{
    SETTING_BOOL,
    SETTING_INT,
    SETTING_FLOAT,
};

enum SettingError
{
    SETTING_NO_ERROR,
    SETTING_WRONG_TYPE,
    SETTING_OUTSIDE_RANGE,
};

enum SettingAllow
{
    SETTING_ALLOW,
    SETTING_STORE,
    SETTING_BLOCK,
    SETTING_ERROR,
};

typedef SettingInit = function bool ();
typedef SettingStart = function bool ();
typedef SettingStop = function void ();
typedef SettingActive = function bool (any value);
typedef SettingChange = function SettingAllow (int client, any value);

bool SettingInitDefault() { return true; }
bool SettingStartDefault() { return true; }
void SettingStopDefault() {}
bool SettingActiveDefaultBool(any value) { return view_as<bool>(value); }
bool SettingActiveDefaultBoolNeg(any value) { return !view_as<bool>(value); }
bool SettingActiveDefaultIntG(any value) { return view_as<int>(value) > 0; }
bool SettingActiveDefaultIntGE(any value) { return view_as<int>(value) >= 0; }
bool SettingActiveDefaultFloatG(any value) { return view_as<float>(value) > 0.0; }
bool SettingActiveDefaultFloatGE(any value) { return view_as<float>(value) >= 0.0; }
SettingAllow SettingChangeDefault(int client, any value) { return SETTING_ALLOW; }

ConVar g_Setting_ConVar_autostop;
enum struct Setting
{
    char name[64];
    char desc[128];
    char expl[256];
    SettingType type;
    float range[2];

    ConVar convar_default;
    ConVar convar_enforce;

    SettingInit f_init;
    SettingStart f_start;
    SettingStop f_stop;

    bool working;

    SettingActive f_active;
    SettingChange f_change;
    bool running;
    int numusing;

    bool active[MAXPLAYERS + 1];
    any values[MAXPLAYERS + 1];
    any prefs[MAXPLAYERS + 1];

    char error_string[256];

    bool Init(any value_default, bool value_enforce)
    {
        if (!this.f_init)
            this.f_init = SettingInitDefault;

        if (!this.f_start)
            this.f_start = SettingStartDefault;

        if (!this.f_stop)
            this.f_stop = SettingStopDefault;

        if (!this.f_active) {
            if (this.type == SETTING_BOOL) {
                this.f_active = SettingActiveDefaultBool;
            }
            else if (this.type == SETTING_INT) {
                if (this.range[0] < 0.0)
                    this.f_active = SettingActiveDefaultIntGE;
                else
                    this.f_active = SettingActiveDefaultIntG;
            }
            else if (this.type == SETTING_FLOAT) {
                if (this.range[0] < 0.0)
                    this.f_active = SettingActiveDefaultFloatGE;
                else
                    this.f_active = SettingActiveDefaultFloatG;
            }
        }

        if (!this.f_change)
            this.f_change = SettingChangeDefault;

        this.running = false;

        this.numusing = 0;
        for (int client = 0; client < MAXPLAYERS + 1; client++)
            this.active[client] = false;

        bool success;
        Call_StartFunction(INVALID_HANDLE, this.f_init);
        Call_Finish(success);

        this.working = success;

        if (!success) {
            LogMessage("Unable to setup setting %s: %s", this.name, g_error);
            return false;
        }

        char string_cmd[128];
        Format(string_cmd, 128, "sm_jumpqol_%s", this.name);
        RegConsoleCmd(string_cmd, Command_Setting);

        char string_value[32];
        if (this.type == SETTING_INT || this.type == SETTING_BOOL)
            IntToString(value_default, string_value, 32);
        else
            FloatToString(value_default, string_value, 32);

        char string_convar[128];
        char string_desc[512];

        if (this.convar_default == null) {
            Format(string_convar, 128, "sm_jumpqol_%s_default", this.name);
            Format(string_desc, 512, "%s\n%s", this.desc, this.expl);
            this.convar_default = CreateConVar(string_convar, string_value, string_desc, _, true, this.range[0], true, this.range[1]);
            this.convar_default.SetString(string_value);
            this.convar_default.AddChangeHook(ConVarChanged_Setting_Default);
        }

        if (this.convar_enforce == null) {
            Format(string_convar, 128, "sm_jumpqol_%s_enforce", this.name);
            IntToString(value_enforce, string_value, 32);
            this.convar_enforce = CreateConVar(string_convar, string_value, "", _, true, 0.0, true, 1.0);
            this.convar_enforce.SetString(string_value);
            this.convar_enforce.AddChangeHook(ConVarChanged_Setting_Enforce);
        }

        return true;
    }

    void Fail()
    {
        if (this.numusing > 0) {
            Call_StartFunction(INVALID_HANDLE, this.f_stop);
            Call_Finish();
        }

        this.working = false;
        this.running = false;
        this.numusing = 0;

        bool setting_default = this.GetDefault();
        for (int client = 1; client <= MAXPLAYERS; client++) {
            this.active[client] = false;
            this.values[client] = setting_default;
            this.prefs[client] = setting_default;
        }
    }

    bool Restart()
    {
        if (!this.Init(this.GetDefault(), this.IsEnforced()))
            return false;

        for (int client = 1; client <= MaxClients; client++)
            if (IsClientInGame(client) && !IsFakeClient(client))
                this.ChangeValue(client, this.prefs[client]);

        return true;
    }

    any GetDefault()
    {
        if (this.type == SETTING_FLOAT)
            return this.convar_default.FloatValue;
        else if (this.type == SETTING_INT || this.type == SETTING_BOOL)
            return this.convar_default.IntValue;
        else
            SetFailState("Invalid setting type.");

        return 0;
    }

    bool IsEnforced()
    {
        return this.convar_enforce.BoolValue;
    }

    SettingError StringToValueEx(const char[] value_string, any& value)
    {
        float value_float;
        bool isfloat = StringToFloatEx(value_string, value_float) > 0;

        int value_int;
        bool isint = isfloat && StringToIntEx(value_string, value_int) > 0;
        isint &= (float(RoundFloat(value_float)) == value_float);

        if (this.type == SETTING_FLOAT)
            value = value_float;
        else if (this.type == SETTING_INT || this.type == SETTING_BOOL)
            value = value_int;

        bool inrange = (this.range[0] <= value_float <= this.range[1]);
        bool is01 = (0.0 <= value_float <= 1.0);

        bool istype = false;
        switch (this.type) {
            case SETTING_FLOAT: {
                istype = isfloat;
            }
            case SETTING_INT: {
                istype = isint;
            }
            case SETTING_BOOL: {
                istype = isint;
                inrange = is01;
            }
        }

        if (!istype)
            return SETTING_WRONG_TYPE;
        else if (!inrange)
            return SETTING_OUTSIDE_RANGE;
        else
            return SETTING_NO_ERROR;
    }

    any StringToValue(const char[] value_string)
    {
        if (this.type == SETTING_FLOAT)
            return StringToFloat(value_string);
        else if (this.type == SETTING_INT || this.type == SETTING_BOOL)
            return StringToInt(value_string);

        return 0;
    }

    char[] ValueToString(any value)
    {
        char value_string[32] = "";

        if (this.type == SETTING_FLOAT)
            FloatToString(value, value_string, sizeof(value_string));
        else if (this.type == SETTING_INT || this.type == SETTING_BOOL)
            IntToString(value, value_string, sizeof(value_string));

        return value_string;
    }

    bool IsActiveValue(any value)
    {
        bool active;
        Call_StartFunction(INVALID_HANDLE, this.f_active);
        Call_PushCell(value);
        Call_Finish(active);

        return active;
    }

    bool SetActive(int client, bool active)
    {
        if (active != this.active[client]) {
            if (active && !this.running) {
                bool success;
                Call_StartFunction(INVALID_HANDLE, this.f_start);
                Call_Finish(success);

                if (!success) {
                    Call_StartFunction(INVALID_HANDLE, this.f_stop);
                    Call_Finish();

                    return false;
                }

                this.running = true;
            }
            else if (!active && this.numusing == 1 && g_Setting_ConVar_autostop.BoolValue) {
                Call_StartFunction(INVALID_HANDLE, this.f_stop);
                Call_Finish();

                this.running = false;
            }

            this.numusing += active ? 1 : -1;
        }

        this.active[client] = active;

        return true;
    }

    SettingAllow ChangeValue(int client, any value, bool setpref = true, bool force = false) {
        SetError("");

        if (!this.working) {
            SetError("This setting is unavailable.");
            return SETTING_ERROR;
        }

        SettingAllow allow = SETTING_ALLOW;
        if (!force) {
            SettingAllow allow_plugin;
            Call_StartFunction(INVALID_HANDLE, this.f_change);
            Call_PushCell(client);
            Call_PushCell(value);
            Call_Finish(allow_plugin);

            if (allow_plugin > allow)
                allow = allow_plugin;

            static GlobalForward s_Call_OnSettingChange;
            if (!s_Call_OnSettingChange)
                s_Call_OnSettingChange = new GlobalForward("Jumpqol_OnSettingChange", ET_Event, Param_String, Param_Cell, Param_Cell, Param_Cell, Param_Cell);

            SettingAllow allow_api;
            Call_StartForward(s_Call_OnSettingChange);
            Call_PushString(this.name);
            Call_PushCell(client);
            Call_PushCell(this.type);
            Call_PushCell(this.values[client]);
            Call_PushCell(value);
            Call_Finish(allow_api);

            if (allow_api > allow)
                allow = allow_api;
        }

        if (allow == SETTING_BLOCK)
            return SETTING_BLOCK;

        any pref_old = this.prefs[client];
        if (setpref)
            this.prefs[client] = value;

        if (allow == SETTING_STORE)
            return SETTING_STORE;

        if (!this.SetActive(client, this.IsActiveValue(value))) {
            this.prefs[client] = pref_old;

            this.working = false;

            LogMessage("Unable to start setting %s: %s", this.name, g_error);

            SetError("This setting is unavailable.");
            return SETTING_ERROR;
        }

        static GlobalForward s_Call_OnSettingChanged;
        if (!s_Call_OnSettingChanged)
            s_Call_OnSettingChanged = new GlobalForward("Jumpqol_OnSettingChanged", ET_Ignore, Param_String, Param_Cell, Param_Cell, Param_Cell, Param_Cell);

        Call_StartForward(s_Call_OnSettingChanged);
        Call_PushString(this.name);
        Call_PushCell(client);
        Call_PushCell(this.type);
        Call_PushCell(this.values[client]);
        Call_PushCell(value);
        Call_Finish();

        this.values[client] = value;

        return allow;
    }
}

enum
{
    SETTING_PROJDECALS,
    SETTING_SHOWDETDECALS,
    SETTING_KEEPBLASTSTATE,
    SETTING_RELOADFIRE,
    SETTING_ATTACK2FIRE,
    SETTING_RAMPFIX,
    SETTING_SLIDEEBFIX,
    SETTING_FLUSHSLOPEFIX,
    SETTING_SYNC,
    SETTING_FAKEDELAY,
    NUM_SETTINGS,
};
Setting g_settings[NUM_SETTINGS];





void ConVarChanged_Setting_Autostop(ConVar convar, const char[] old_value, const char[] new_value)
{
    bool autostop = !!StringToInt(new_value);
    if (!autostop)
        return;

    for (int setting = 0; setting < NUM_SETTINGS; setting++) {
        if (g_settings[setting].running && g_settings[setting].numusing == 0) {
            Call_StartFunction(INVALID_HANDLE, g_settings[setting].f_stop);
            Call_Finish();

            g_settings[setting].running = false;
        }
    }
}





/*
_api
 █████  ██████  ██
██   ██ ██   ██ ██
███████ ██████  ██
██   ██ ██      ██
██   ██ ██      ██
*/





int NameToSetting(const char[] name)
{
    for (int setting = 0; setting < NUM_SETTINGS; setting++)
        if (StrEqual(name, g_settings[setting].name, false))
            return setting;

    return -1;
}

public APLRes AskPluginLoad2(Handle myself, bool late, char[] error, int err_max)
{
    CreateNative("Jumpqol_GetSettingCount", GetSettingCount);
    CreateNative("Jumpqol_GetSettingName", GetSettingName);
    CreateNative("Jumpqol_GetSettingType", GetSettingType);
    CreateNative("Jumpqol_GetSettingRange", GetSettingRange);
    CreateNative("Jumpqol_SetSettingValue", SetSettingValue);
    CreateNative("Jumpqol_GetSettingValue", GetSettingValue);
    CreateNative("Jumpqol_ApplyPreferredValue", ApplyPreferredValue);
    CreateNative("Jumpqol_ApplyPreferredValues", ApplyPreferredValues);

    return APLRes_Success;
}

any GetSettingCount(Handle plugin, int numparams)
{
    return NUM_SETTINGS;
}

any GetSettingName(Handle plugin, int numparams)
{
    int setting = GetNativeCell(1);
    int maxlength = GetNativeCell(3);
    SetNativeString(2, g_settings[setting].name, maxlength);

    return 0;
}

any GetSettingType(Handle plugin, int numparams)
{
    char name[64];
    GetNativeString(1, name, 64);

    int setting = NameToSetting(name);
    return g_settings[setting].type;
}

any GetSettingRange(Handle plugin, int numparams)
{
    char name[64];
    GetNativeString(1, name, 64);

    int setting = NameToSetting(name);
    SetNativeArray(2, g_settings[setting].range, 2);

    return 0;
}

any SetSettingValue(Handle plugin, int numparams)
{
    char name[64];
    GetNativeString(1, name, 64);

    int client = GetNativeCell(2);
    any value = GetNativeCell(3);

    bool force = GetNativeCell(4);

    int setting = NameToSetting(name);
    return g_settings[setting].ChangeValue(client, value, false, force) == SETTING_ALLOW;
}

any GetSettingValue(Handle plugin, int numparams)
{
    char name[64];
    GetNativeString(1, name, 64);

    int client = GetNativeCell(2);

    int setting = NameToSetting(name);
    return g_settings[setting].values[client];
}

any ApplyPreferredValue(Handle plugin, int numparams)
{
    char name[64];
    GetNativeString(1, name, 64);

    int client = GetNativeCell(2);

    bool force = GetNativeCell(3);

    int setting = NameToSetting(name);
    return g_settings[setting].ChangeValue(client, g_settings[setting].prefs[client], false, force) == SETTING_ALLOW;
}

any ApplyPreferredValues(Handle plugin, int numparams)
{
    int client = GetNativeCell(1);

    bool force = GetNativeCell(2);

    bool error = false;
    for (int setting = 0; setting < NUM_SETTINGS; setting++)
        if (g_settings[setting].ChangeValue(client, g_settings[setting].prefs[client], false, force) != SETTING_ALLOW)
            error = true;

    return !error;
}





/*
_projectile
██████  ██████   ██████       ██ ███████  ██████ ████████ ██ ██      ███████
██   ██ ██   ██ ██    ██      ██ ██      ██         ██    ██ ██      ██     
██████  ██████  ██    ██      ██ █████   ██         ██    ██ ██      █████  
██      ██   ██ ██    ██ ██   ██ ██      ██         ██    ██ ██      ██     
██      ██   ██  ██████   █████  ███████  ██████    ██    ██ ███████ ███████
*/





int GetOwner(int entity)
{
    int owner = GetEntPropEnt(entity, Prop_Data, "m_hOwnerEntity");
    if (owner == -1 && HasEntProp(entity, Prop_Send, "m_hThrower"))
        owner = GetEntPropEnt(entity, Prop_Send, "m_hThrower");
    else if (owner != -1 && HasEntProp(owner, Prop_Send, "m_hBuilder"))
        owner = GetEntPropEnt(owner, Prop_Send, "m_hBuilder");

    return owner;
}





enum struct ProjectileState
{
    int frame;

    float pos[3];
    float rot[3];
    float vel[3];
    float angvel[3];

    void ReadFrom(int entity)
    {
        GetEntDataVector(entity, FindDataMapInfo(0, "m_vecOrigin"), this.pos);
        GetEntDataVector(entity, FindDataMapInfo(0, "m_angRotation"), this.rot);
        GetEntDataVector(entity, FindDataMapInfo(0, "m_vecVelocity"), this.vel);
        GetEntDataVector(entity, FindDataMapInfo(0, "m_vecAngVelocity"), this.angvel);
    }

    void WriteTo(int entity)
    {
        SetEntDataVector(entity, FindDataMapInfo(0, "m_vecOrigin"), this.pos, true);
        SetEntDataVector(entity, FindDataMapInfo(0, "m_angRotation"), this.rot, true);
        SetEntDataVector(entity, FindDataMapInfo(0, "m_vecVelocity"), this.vel, true);
        SetEntDataVector(entity, FindDataMapInfo(0, "m_vecAngVelocity"), this.angvel, true);
    }
}

#define NUM_PREDICTIONS 20 // 300 ms prediction in both directions
#define NUM_BUFFER      (3*NUM_PREDICTIONS)
#define MAX_PROJECTILES 32
ProjectileState g_preds[MAXPLAYERS*MAX_PROJECTILES][NUM_BUFFER];
ArrayList g_pred_indices;

enum struct Projectile
{
    int ref;

    MoveType movetype;

    bool updated;

    int frame;
    int buffer;

    int frame_manipulated;

    int clientdelay;

    int Entity()
    {
        return EntRefToEntIndex(this.ref);
    }

    bool IsMoveTypeSupported()
    {
        return this.movetype == MOVETYPE_FLY;
    }

    bool HasBaseVelocity()
    {
        float basevel[3];
        GetEntDataVector(this.Entity(), FindDataMapInfo(0, "m_vecBaseVelocity"), basevel);
        return !CompareVectors(basevel, {0.0, 0.0, 0.0});
    }

    bool IsManipulated()
    {
        return g_tickcount_frame <= this.frame_manipulated + 3; // 3 extra frames for good measure
    }

    bool IsPredictable()
    {
        return this.IsMoveTypeSupported() && !this.HasBaseVelocity() && !this.IsManipulated();
    }

    void Init(int ref)
    {
        int entity = EntRefToEntIndex(ref);

        this.ref = ref;
        this.movetype = view_as<MoveType>(GetEntProp(entity, Prop_Data, "m_MoveType"));
        this.frame_manipulated = 0;
    }

    void Activate()
    {
        this.buffer = g_pred_indices.Get(g_pred_indices.Length - 1);
        g_pred_indices.Resize(g_pred_indices.Length - 1);

        this.ResetPredictions();
    }

    void Deactivate()
    {
        g_pred_indices.Push(this.buffer);
        this.buffer = -1;
    }

    void ResetPredictions()
    {
        this.frame = g_tickcount_frame;
        this.StoreCurrentState();
    }

    void Update(bool force)
    {
        // Already updated this frame
        if (this.updated && !force)
            return;

        this.updated = true;

        if (force)
            this.frame++;
        else
            this.frame = g_tickcount_frame;

        this.StoreCurrentState();
    }

    void StoreCurrentState()
    {
        ProjectileState state_current;
        state_current.ReadFrom(this.Entity());
        state_current.frame = this.frame;

        ProjectileState state_predicted;
        state_predicted = g_preds[this.buffer][this.frame % NUM_BUFFER];

        bool same = true;
        same &= (state_predicted.frame == state_current.frame);
        same &= CompareVectors(state_current.pos, state_predicted.pos);
        same &= CompareVectors(state_current.rot, state_predicted.rot);
        same &= CompareVectors(state_current.vel, state_predicted.vel);
        same &= CompareVectors(state_current.angvel, state_predicted.angvel);

        g_preds[this.buffer][this.frame % NUM_BUFFER] = state_current;

        if (!same) {
            ProjectileState state;

            state = state_current;
            for (int pframe = this.frame - 1; pframe >= this.frame - NUM_PREDICTIONS; pframe--) {
                state = this.PredictBackwards(state);
                g_preds[this.buffer][pframe % NUM_BUFFER] = state;
            }

            state = state_current;
            for (int pframe = this.frame + 1; pframe <= this.frame + NUM_PREDICTIONS; pframe++) {
                state = this.PredictForward(state);
                g_preds[this.buffer][pframe % NUM_BUFFER] = state;
            }
        }
        else {
            ProjectileState state;
            state = g_preds[this.buffer][(this.frame + NUM_PREDICTIONS - 1) % NUM_BUFFER];
            state = this.PredictForward(state);
            g_preds[this.buffer][(this.frame + NUM_PREDICTIONS) % NUM_BUFFER] = state;
        }
    }

    ProjectileState GetState(int frame)
    {
        ProjectileState state;
        state = g_preds[this.buffer][frame % NUM_BUFFER];
        if (state.frame != frame)
            state = g_preds[this.buffer][g_tickcount_frame % NUM_BUFFER];

        return state;
    }

    ProjectileState PredictForward(ProjectileState current)
    {
        ProjectileState state;

        if (this.movetype == MOVETYPE_FLY) {
            state.vel = current.vel;
            state.angvel = current.angvel;

            OffsetVector(current.pos, current.vel, g_globals.interval_per_tick, state.pos);
            OffsetVector(current.rot, current.angvel, g_globals.interval_per_tick, state.rot);

            state.frame = current.frame + 1;
        }

        return state;
    }

    ProjectileState PredictBackwards(ProjectileState current)
    {
        ProjectileState state;

        if (this.movetype == MOVETYPE_FLY) {
            state.vel = current.vel;
            state.angvel = current.angvel;

            OffsetVector(current.pos, current.vel, -g_globals.interval_per_tick, state.pos);
            OffsetVector(current.rot, current.angvel, -g_globals.interval_per_tick, state.rot);

            state.frame = current.frame - 1;
        }

        return state;
    }
}
Projectile g_projs[MAXPLAYERS+1][MAX_PROJECTILES];
int g_numprojs[MAXPLAYERS+1];

int g_delprojs[MAXPLAYERS+1][MAX_PROJECTILES];
int g_numdelprojs[MAXPLAYERS+1];

int g_newprojs[MAXPLAYERS+1][MAX_PROJECTILES];
int g_numnewprojs[MAXPLAYERS+1];

enum struct ProjectileInfo
{
    int ref;
    int client;
    int index;
}

#define MAX_EDICTS (1<<11)
int g_findclients[MAX_EDICTS] = {-1, ...};
ProjectileInfo FindProjectile(int entity)
{
    ProjectileInfo info = {-1, -1, -1};

    int ref = EntIndexToEntRef(entity);
    if (ref == -1)
        return info;

    int client = g_findclients[entity];
    if ( !(1 <= client <= MaxClients) )
        return info;

    int numprojs = g_numprojs[client];
    for (int index = 0; index < numprojs; index++) {
        if (ref == g_projs[client][index].ref) {
            info.ref = ref;
            info.client = client;
            info.index = index;
            return info;
        }
    }

    return info;
}





/*
_session
███████ ███████ ███████ ███████ ██  ██████  ███    ██
██      ██      ██      ██      ██ ██    ██ ████   ██
███████ █████   ███████ ███████ ██ ██    ██ ██ ██  ██
     ██ ██           ██      ██ ██ ██    ██ ██  ██ ██
███████ ███████ ███████ ███████ ██  ██████  ██   ████
*/





enum struct ClientSettings
{
    float cmdrate;
    float updaterate;
    float interp_ratio;
    float interp;
}
ClientSettings g_clientsettings[MAXPLAYERS+1];

void CallbackClientSetting(QueryCookie cookie, int client, ConVarQueryResult result, const char[] cvarName, const char[] cvarValue)
{
    float value = StringToFloat(cvarValue);

    if (StrEqual(cvarName, "cl_cmdrate")) {
        float min = FindConVar("sv_mincmdrate").FloatValue;
        float max = FindConVar("sv_maxcmdrate").FloatValue;
        g_clientsettings[client].cmdrate = FloatClamp(value, min, max);
    }
    else if (StrEqual(cvarName, "cl_updaterate")) {
        float min = FindConVar("sv_minupdaterate").FloatValue;
        float max = FindConVar("sv_maxupdaterate").FloatValue;
        g_clientsettings[client].updaterate = FloatClamp(value, min, max);
    }
    else if (StrEqual(cvarName, "cl_interp_ratio")) {
        float min = FindConVar("sv_client_min_interp_ratio").FloatValue;
        float max = FindConVar("sv_client_max_interp_ratio").FloatValue;
        g_clientsettings[client].interp_ratio = FloatClamp(value, min, max);
    }
    else if (StrEqual(cvarName, "cl_interp")) {
        g_clientsettings[client].interp = value;
    }
}

public void OnClientSettingsChanged(int client)
{
    if (!IsClientInGame(client))
        return;

    QueryClientConVar(client, "cl_cmdrate", CallbackClientSetting);
    QueryClientConVar(client, "cl_updaterate", CallbackClientSetting);
    QueryClientConVar(client, "cl_interp_ratio", CallbackClientSetting);
    QueryClientConVar(client, "cl_interp", CallbackClientSetting);
}

// Following UTIL_GetPlayerConnectionInfo
float GetLatency(int client)
{
    return GetClientAvgLatency(client, NetFlow_Outgoing) - 0.5/g_clientsettings[client].cmdrate - 1.0*g_globals.interval_per_tick - 0.5*g_globals.interval_per_tick;
}

int GetDelay(int client)
{
    float interp = FloatMax(g_clientsettings[client].interp, g_clientsettings[client].interp_ratio / g_clientsettings[client].updaterate);

    // Following UTIL_GetPlayerConnectionInfo
    float latency = GetLatency(client);

    int latency_frames = latency > 0.0 ? RoundToCeil(latency / g_globals.interval_per_tick) : 1;
    int interp_frames = RoundToNearest(interp / g_globals.interval_per_tick);

    return latency_frames + interp_frames;
}





bool g_chargemsg[MAXPLAYERS+1];

methodmap Session
{
    public Session(int client)
    {
        return view_as<Session>(client);
    }

    property int client
    {
        public get()
        {
            return view_as<int>(this);
        }
    }

    public void Init()
    {
        this.Clear();

        if (IsClientInGame(this.client)) {
            this.OnConnected();
            this.OnPutInServer();
        }
    }

    public void Clear()
    {
        this.ResetChargeMessage();
        this.ClearProjectiles();
    }

    public void ApplyPreferences()
    {
        for (int setting = 0; setting < NUM_SETTINGS; setting++)
            g_settings[setting].ChangeValue(this.client, g_settings[setting].prefs[this.client]);
    }

    public void OnConnected()
    {
        if (IsFakeClient(this.client))
            return;

        this.Clear();

        for (int setting = 0; setting < NUM_SETTINGS; setting++)
            if (g_settings[setting].working)
                g_settings[setting].prefs[this.client] = g_settings[setting].GetDefault();

        this.ApplyPreferences();
    }

    public void OnPutInServer()
    {
        if (IsFakeClient(this.client))
            return;

        g_clientsettings[this.client].cmdrate = 66.0;
        g_clientsettings[this.client].updaterate = 66.0;
        g_clientsettings[this.client].interp_ratio = 1.0;
        g_clientsettings[this.client].interp = 0.015;
        OnClientSettingsChanged(this.client);
    }

    public void OnDisconnect()
    {
        if (IsFakeClient(this.client))
            return;

        this.Clear();

        for (int setting = 0; setting < NUM_SETTINGS; setting++)
            g_settings[setting].SetActive(this.client, false);
    }

    public void ResetChargeMessage()
    {
        g_chargemsg[this.client] = false;
    }

    public void ClearProjectiles()
    {
        for (int i = 0; i < g_numprojs[this.client]; i++)
            g_projs[this.client][i].Deactivate();

        g_numprojs[this.client] = 0;
        g_numnewprojs[this.client] = 0;
    }

    public int FindProjectile(int ref)
    {
        int client = this.client;
        for (int index = 0; index < g_numprojs[client]; index++)
            if (g_projs[client][index].ref == ref)
                return index;

        return -1;
    }

    public bool HasProjectile(int ref)
    {
        return this.FindProjectile(ref) != -1;
    }

    public bool AddProjectile(int ref)
    {
        int client = this.client;

        if (g_numprojs[client] == MAX_PROJECTILES || this.HasProjectile(ref))
            return false;

        int index = g_numprojs[client];

        g_numprojs[client]++;

        g_projs[client][index].Init(ref);
        g_projs[client][index].Activate();
        g_projs[client][index].clientdelay = GetDelay(client);

        return true;
    }

    public bool RemoveProjectile(int ref)
    {
        int client = this.client;

        int index = this.FindProjectile(ref);
        if (index == -1)
            return false;

        g_projs[client][index].Deactivate();

        for (int i = index; i < g_numprojs[client] - 1; i++)
            g_projs[client][i] = g_projs[client][i + 1];

        g_numprojs[client]--;

        return true;
    }

    property bool projdecals
    {
        public get()
        {
            return g_settings[SETTING_PROJDECALS].values[this.client];
        }
    }

    property int showdetdecals
    {
        public get()
        {
            return g_settings[SETTING_SHOWDETDECALS].values[this.client];
        }
    }

    property bool keepblaststate
    {
        public get()
        {
            return g_settings[SETTING_KEEPBLASTSTATE].values[this.client];
        }
    }

    property bool reloadfire
    {
        public get()
        {
            return g_settings[SETTING_RELOADFIRE].values[this.client];
        }
    }

    property int attack2fire
    {
        public get()
        {
            return g_settings[SETTING_ATTACK2FIRE].values[this.client];
        }
    }

    property bool rampfix
    {
        public get()
        {
            return g_settings[SETTING_RAMPFIX].values[this.client];
        }
    }

    property bool slideebfix
    {
        public get()
        {
            return g_settings[SETTING_SLIDEEBFIX].values[this.client];
        }
    }

    property int flushslopefix
    {
        public get()
        {
            return g_settings[SETTING_FLUSHSLOPEFIX].values[this.client];
        }
    }

    property bool sync
    {
        public get()
        {
            return g_settings[SETTING_SYNC].values[this.client];
        }
    }

    property int fakedelay
    {
        public get()
        {
            return g_settings[SETTING_FAKEDELAY].values[this.client];
        }
    }
}
Session g_sessions[MAXPLAYERS+1];





/*
_event
███████ ██    ██ ███████ ███    ██ ████████
██      ██    ██ ██      ████   ██    ██   
█████   ██    ██ █████   ██ ██  ██    ██   
██       ██  ██  ██      ██  ██ ██    ██   
███████   ████   ███████ ██   ████    ██   
*/





public void OnClientConnected(int client)
{
    g_sessions[client].OnConnected();
}

public void OnClientPutInServer(int client)
{
    g_sessions[client].OnPutInServer();
}

public void OnClientDisconnect(int client)
{
    g_sessions[client].OnDisconnect();
}

void OnClientDeath(Event event, const char[] name, bool dontBroadcast)
{
    int client = GetClientOfUserId(GetEventInt(event, "userid"));

    g_sessions[client].Clear();
}

void OnClientChangeClass(Event event, const char[] name, bool dontBroadcast)
{
    int client = GetClientOfUserId(GetEventInt(event, "userid"));

    g_sessions[client].Clear();
}





/*
_init
██ ███    ██ ██ ████████
██ ████   ██ ██    ██   
██ ██ ██  ██ ██    ██   
██ ██  ██ ██ ██    ██   
██ ██   ████ ██    ██   
*/





public void OnPluginStart()
{
    g_gameconf = LoadGameConfigFile("jumpqol");
    if (!g_gameconf)
        SetFailState("[jumpqol.smx] Unable to start plugin: Failed to load gameconf jumpqol.txt.");

    g_sendvaredit_loaded = LibraryExists("sendvaredit");

    // Detours
    g_detours[DETOUR_PHYSICS_SIMULATEENTITY].Init(
        "Physics_SimulateEntity",
        CallConv_CDECL, ReturnType_Void, ThisPointer_Ignore,
        {HookParamType_CBaseEntity, HookParamType_Unknown}
    );
    g_detours[DETOUR_SERVICEEVENTS].Init(
        "CEventQueue::ServiceEvents",
        CallConv_THISCALL, ReturnType_Void, ThisPointer_Address,
        {HookParamType_Unknown}
    );
    g_detours[DETOUR_ITEMPOSTFRAME].Init(
        "CTFWeaponBase::ItemPostFrame",
        CallConv_THISCALL, ReturnType_Void, ThisPointer_CBaseEntity,
        {HookParamType_Unknown}
    );
    g_detours[DETOUR_UTIL_DECALTRACE].Init(
        "UTIL_DecalTrace",
        CallConv_CDECL, ReturnType_Void, ThisPointer_Ignore,
        {HookParamType_Int, HookParamType_CharPtr, HookParamType_Unknown}
    );
    g_detours[DETOUR_TFPLAYERTHINK].Init(
        "CTFPlayer::TFPlayerThink",
        CallConv_THISCALL, ReturnType_Void, ThisPointer_CBaseEntity,
        {HookParamType_Unknown}
    );
    g_detours[DETOUR_ITEMBUSYFRAME].Init(
        "CTFWeaponBase::ItemBusyFrame",
        CallConv_THISCALL, ReturnType_Void, ThisPointer_CBaseEntity,
        {HookParamType_Unknown}
    );
    g_detours[DETOUR_CLIENT_ITEMPOSTFRAME].Init(
        "client CBasePlayer::ItemPostFrame",
        CallConv_THISCALL, ReturnType_Void, ThisPointer_Address,
        {HookParamType_Unknown}
    );
    g_detours[DETOUR_CLIENT_ITEMBUSYFRAME].Init(
        "client CTFWeaponBase::ItemBusyFrame",
        CallConv_THISCALL, ReturnType_Void, ThisPointer_Address,
        {HookParamType_Unknown}
    );
    g_detours[DETOUR_ITEMBUSYFRAME].Init(
        "CTFWeaponBase::ItemBusyFrame",
        CallConv_THISCALL, ReturnType_Void, ThisPointer_CBaseEntity,
        {HookParamType_Unknown}
    );
    g_detours[DETOUR_SETGROUNDENTITY].Init(
        "CTFGameMovement::SetGroundEntity",
        CallConv_THISCALL, ReturnType_Void, ThisPointer_Address,
        {HookParamType_ObjectPtr, HookParamType_Unknown}
    );
    g_detours[DETOUR_CM_CLIPBOXTOBRUSH].Init(
        "CM_ClipBoxToBrush<false>",
        CallConv_CDECL, ReturnType_Void, ThisPointer_Ignore,
        {HookParamType_ObjectPtr, HookParamType_ObjectPtr, HookParamType_Unknown}
    );

    // Required
    if (!Required_Init())
        SetFailState("[jumpqol.smx] Unable to start plugin: %s", g_error);

    // Settings
    RegConsoleCmd("sm_jumpqol", Command_Plugin);

    g_Setting_ConVar_autostop = CreateConVar(
        "sm_jumpqol_setting_autostop",
        "0",
        "Controls if a setting should stop running when nobody is using it.\nDetours changing page permissions can seemingly lead to lag on virtualized servers, so this exists to mitigate that.",
        _,
        true,
        0.0,
        true,
        1.0
    );
    g_Setting_ConVar_autostop.AddChangeHook(ConVarChanged_Setting_Autostop);

    g_settings[SETTING_PROJDECALS].name = "projdecals";
    g_settings[SETTING_PROJDECALS].desc = "Controls if decals should be elongated when a prop is hit at an angle.";
    g_settings[SETTING_PROJDECALS].expl = "";
    g_settings[SETTING_PROJDECALS].type = SETTING_BOOL;
    g_settings[SETTING_PROJDECALS].f_start = Projdecals_Start;
    g_settings[SETTING_PROJDECALS].f_stop = Projdecals_Stop;
    g_settings[SETTING_PROJDECALS].f_active = SettingActiveDefaultBoolNeg;
    g_settings[SETTING_PROJDECALS].range[0] = 0.0;
    g_settings[SETTING_PROJDECALS].range[1] = 1.0;
    g_settings[SETTING_PROJDECALS].Init(false, false);

    g_settings[SETTING_SHOWDETDECALS].name = "showdetdecals";
    g_settings[SETTING_SHOWDETDECALS].desc = "Makes explosives you manually detonate produce decals.";
    g_settings[SETTING_SHOWDETDECALS].expl = "";
    g_settings[SETTING_SHOWDETDECALS].type = SETTING_BOOL;
    g_settings[SETTING_SHOWDETDECALS].f_init = Showdetdecals_Init;
    g_settings[SETTING_SHOWDETDECALS].f_start = Showdetdecals_Start;
    g_settings[SETTING_SHOWDETDECALS].f_stop = Showdetdecals_Stop;
    g_settings[SETTING_SHOWDETDECALS].f_active = SettingActiveDefaultBool;
    g_settings[SETTING_SHOWDETDECALS].range[0] = 0.0;
    g_settings[SETTING_SHOWDETDECALS].range[1] = 1.0;
    g_settings[SETTING_SHOWDETDECALS].Init(false, false);

    g_settings[SETTING_KEEPBLASTSTATE].name = "keepblaststate";
    g_settings[SETTING_KEEPBLASTSTATE].desc = "Prevents the blast state from getting cleared when using an explosion to leave the ground.";
    g_settings[SETTING_KEEPBLASTSTATE].expl = "";
    g_settings[SETTING_KEEPBLASTSTATE].type = SETTING_BOOL;
    g_settings[SETTING_KEEPBLASTSTATE].f_start = Keepblaststate_Start;
    g_settings[SETTING_KEEPBLASTSTATE].f_stop = Keepblaststate_Stop;
    g_settings[SETTING_KEEPBLASTSTATE].f_active = SettingActiveDefaultBool;
    g_settings[SETTING_KEEPBLASTSTATE].range[0] = 0.0;
    g_settings[SETTING_KEEPBLASTSTATE].range[1] = 1.0;
    g_settings[SETTING_KEEPBLASTSTATE].Init(true, false);

    g_settings[SETTING_RELOADFIRE].name = "reloadfire";
    g_settings[SETTING_RELOADFIRE].desc = "Makes you shoot on the same tick you reload canceled (no firing sound if you only press for 1 tick).";
    g_settings[SETTING_RELOADFIRE].expl = "";
    g_settings[SETTING_RELOADFIRE].type = SETTING_BOOL;
    g_settings[SETTING_RELOADFIRE].f_init = Reloadfire_Init;
    g_settings[SETTING_RELOADFIRE].f_start = Reloadfire_Start;
    g_settings[SETTING_RELOADFIRE].f_stop = Reloadfire_Stop;
    g_settings[SETTING_RELOADFIRE].f_active = SettingActiveDefaultBool;
    g_settings[SETTING_RELOADFIRE].range[0] = 0.0;
    g_settings[SETTING_RELOADFIRE].range[1] = 1.0;
    g_settings[SETTING_RELOADFIRE].Init(true, false);

    g_settings[SETTING_ATTACK2FIRE].name = "attack2fire";
    g_settings[SETTING_ATTACK2FIRE].desc = "Lets you shoot rockets while attack2 is pressed.";
    g_settings[SETTING_ATTACK2FIRE].expl = "0: Disable.\n1: Block attack2 from all primary rocket launchers.\n2: Allow Cow Mangler 5000 charged shots.";
    g_settings[SETTING_ATTACK2FIRE].type = SETTING_INT;
    g_settings[SETTING_ATTACK2FIRE].f_init = Attack2fire_Init;
    g_settings[SETTING_ATTACK2FIRE].f_start = Attack2fire_Start;
    g_settings[SETTING_ATTACK2FIRE].f_stop = Attack2fire_Stop;
    g_settings[SETTING_ATTACK2FIRE].f_active = SettingActiveDefaultIntG;
    g_settings[SETTING_ATTACK2FIRE].range[0] = 0.0;
    g_settings[SETTING_ATTACK2FIRE].range[1] = 2.0;
    g_settings[SETTING_ATTACK2FIRE].Init(1, false);

    g_settings[SETTING_RAMPFIX].name = "rampfix";
    g_settings[SETTING_RAMPFIX].desc = "Prevents the event where you sometimes stop up on a ramp you should have been able to slide up.";
    g_settings[SETTING_RAMPFIX].expl = "";
    g_settings[SETTING_RAMPFIX].type = SETTING_BOOL;
    g_settings[SETTING_RAMPFIX].f_start = Rampfix_Start;
    g_settings[SETTING_RAMPFIX].f_stop = Rampfix_Stop;
    g_settings[SETTING_RAMPFIX].f_active = SettingActiveDefaultBool;
    g_settings[SETTING_RAMPFIX].range[0] = 0.0;
    g_settings[SETTING_RAMPFIX].range[1] = 1.0;
    g_settings[SETTING_RAMPFIX].Init(true, false);

    g_settings[SETTING_SLIDEEBFIX].name = "slideebfix";
    g_settings[SETTING_SLIDEEBFIX].desc = "Prevents you from somtimes not getting an edgebug on an auto edgebug with a slide setup.";
    g_settings[SETTING_SLIDEEBFIX].expl = "";
    g_settings[SETTING_SLIDEEBFIX].type = SETTING_BOOL;
    g_settings[SETTING_SLIDEEBFIX].f_init = Slideebfix_Init;
    g_settings[SETTING_SLIDEEBFIX].f_start = Slideebfix_Start;
    g_settings[SETTING_SLIDEEBFIX].f_stop = Slideebfix_Stop;
    g_settings[SETTING_SLIDEEBFIX].f_active = SettingActiveDefaultBool;
    g_settings[SETTING_SLIDEEBFIX].range[0] = 0.0;
    g_settings[SETTING_SLIDEEBFIX].range[1] = 1.0;
    g_settings[SETTING_SLIDEEBFIX].Init(true, false);

    g_settings[SETTING_FLUSHSLOPEFIX].name = "flushslopefix";
    g_settings[SETTING_FLUSHSLOPEFIX].desc = "Prevents you colliding with hidden sides of sloped brushes and optionally removes pixelwalking.";
    g_settings[SETTING_FLUSHSLOPEFIX].expl = "0: No prevention.\n1: Prevent some bugs on ramps made of multiple brushes.\n2: Prevent pixelwalking too.";
    g_settings[SETTING_FLUSHSLOPEFIX].type = SETTING_INT;
    g_settings[SETTING_FLUSHSLOPEFIX].f_start = Flushslopefix_Start;
    g_settings[SETTING_FLUSHSLOPEFIX].f_stop = Flushslopefix_Stop;
    g_settings[SETTING_FLUSHSLOPEFIX].f_active = SettingActiveDefaultIntG;
    g_settings[SETTING_FLUSHSLOPEFIX].range[0] = 0.0;
    g_settings[SETTING_FLUSHSLOPEFIX].range[1] = 2.0;
    g_settings[SETTING_FLUSHSLOPEFIX].Init(0, false);

    g_settings[SETTING_SYNC].name = "sync";
    g_settings[SETTING_SYNC].desc = "Fixes rockets getting desynced with the shooter to remove randomness.";
    g_settings[SETTING_SYNC].expl = "";
    g_settings[SETTING_SYNC].type = SETTING_BOOL;
    g_settings[SETTING_SYNC].f_init = Sync_Init;
    g_settings[SETTING_SYNC].f_start = Sync_Start;
    g_settings[SETTING_SYNC].f_stop = Sync_Stop;
    g_settings[SETTING_SYNC].f_active = SettingActiveDefaultBool;
    g_settings[SETTING_SYNC].range[0] = 0.0;
    g_settings[SETTING_SYNC].range[1] = 1.0;
    g_settings[SETTING_SYNC].Init(true, false);

    g_settings[SETTING_FAKEDELAY].name = "fakedelay";
    g_settings[SETTING_FAKEDELAY].desc = "Tries to display rockets as if you were on the specified delay (scoreboard ping + net_graph lerp).";
    g_settings[SETTING_FAKEDELAY].expl = "-1: Disable.\n0 or above: Delay to fake in ms.";
    g_settings[SETTING_FAKEDELAY].type = SETTING_INT;
    g_settings[SETTING_FAKEDELAY].f_init = Fakedelay_Init;
    g_settings[SETTING_FAKEDELAY].f_active = SettingActiveDefaultIntGE;
    g_settings[SETTING_FAKEDELAY].range[0] = -1.0;
    g_settings[SETTING_FAKEDELAY].range[1] = NUM_PREDICTIONS*(g_globals.interval_per_tick*1000.0);
    g_settings[SETTING_FAKEDELAY].Init(-1, false);

    // Config
    AutoExecConfig(true, "jumpqol");

    if (!IsDedicatedServer())
        g_settings[SETTING_ATTACK2FIRE].working = false;

    HookEvent("player_death", OnClientDeath);
    HookEvent("player_changeclass", OnClientChangeClass);

    for (int client = 1; client <= MaxClients; client++) {
        g_sessions[client] = Session(client);
        g_sessions[client].Init();
    }

    g_pred_indices = new ArrayList(1, MAXPLAYERS*MAX_PROJECTILES);
    for (int index = 0; index < MAXPLAYERS*MAX_PROJECTILES; index++)
        g_pred_indices.Set(index, index);

    #if DEBUG
    RegConsoleCmd("sm_jumpqol_debug", Command_Debug);
    #endif
}

public void OnLibraryAdded(const char[] name)
{
    if (StrEqual(name, "sendvaredit")) {
        g_sendvaredit_loaded = true;

        if (!g_settings[SETTING_SYNC].working)
            g_settings[SETTING_SYNC].Restart();

        if (!g_settings[SETTING_FAKEDELAY].working)
            g_settings[SETTING_FAKEDELAY].Restart();
    }
}

public void OnLibraryRemoved(const char[] name)
{
    if (StrEqual(name, "sendvaredit")) {
        g_sendvaredit_loaded = false;

        if (g_settings[SETTING_SYNC].working)
            g_settings[SETTING_SYNC].Fail();

        if (g_settings[SETTING_FAKEDELAY].working)
            g_settings[SETTING_FAKEDELAY].Fail();
    }
}





/*
_debug
██████  ███████ ██████  ██    ██  ██████ 
██   ██ ██      ██   ██ ██    ██ ██      
██   ██ █████   ██████  ██    ██ ██   ███
██   ██ ██      ██   ██ ██    ██ ██    ██
██████  ███████ ██████   ██████   ██████ 
*/





#if DEBUG
int g_debug_output = 0;
int g_debug_target = -1;

enum DebugOption
{
    DEBUG_NUMPROJS,
    DEBUG_PRINT,
    DEBUG_UPDATE,
    DEBUG_MANIP,
    DEBUG_SYNC,
    DEBUG_NUM,
}
char debug_names[DEBUG_NUM][64] = {"numprojs", "", "update", "manip", "sync"};
bool debug_toggles[DEBUG_NUM];

Action Command_Debug(int client, int args)
{
    if (args == 0) {
        ReplyToCommand(client, "Usage: sm_jumpqol_debug <target/option>\nDisable toggles with target = -1.\nAvailable options:\n(print) numprojs - Print number of tracked projectiles.\n(toggle) update - Print state of projectile after an update.\n(toggle) manip - Print when a projectile is manipulated by a trigger or event.\n(toggle) sync - Print sync variables.");
        return Plugin_Handled;
    }

    g_debug_output = client;

    char choice[64];
    GetCmdArg(1, choice, sizeof(choice));

    int target;
    if (StringToIntEx(choice, target) != 0) {
        if (target != -1 && !IsActivePlayer(target))
            ReplyToCommand(client, "Invalid target.");
        else
            g_debug_target = target;

        return Plugin_Handled;
    }

    DebugOption option = DEBUG_NUM;
    for (int i = 0; i < view_as<int>(DEBUG_NUM); i++) {
        if (StrEqual(choice, debug_names[i], false)) {
            option = view_as<DebugOption>(i);
            break;
        }
    }

    if (option == DEBUG_NUM) {
        ReplyToCommand(client, "Invalid option.");
        return Plugin_Handled;
    }

    if (g_debug_target == -1) {
        ReplyToCommand(client, "No target set.");
        return Plugin_Handled;
    }

    if (option == DEBUG_NUMPROJS) {
        ReplyToCommand(g_debug_output, "%d", g_numprojs[g_debug_target]);
    }
    else if (option < DEBUG_PRINT) {
        if (g_numprojs[g_debug_target] == 0) {
            ReplyToCommand(g_debug_output, "No active projectiles for target.");
            return Plugin_Handled;
        }

        // Projectile proj;
        // proj = g_projs[g_debug_target][0];
        // if (option == DEBUG_STATE) {
        //     ReplyToCommand(g_debug_output, "movetype: %s, basevel: %s, manipulated: %s, last manip frame %d, current frame: %d", proj.IsMoveTypeSupported() ? "supported" : "unsupported", proj.HasBaseVelocity() ? "yes" : "no", proj.IsManipulated() ? "yes" : "no", proj.frame_manipulated, proj.frame);
        // }
    }
    else {
        debug_toggles[option] ^= true;
    }

    return Plugin_Handled;
}
#endif





/*
_command
 ██████  ██████  ███    ███ ███    ███  █████  ███    ██ ██████ 
██      ██    ██ ████  ████ ████  ████ ██   ██ ████   ██ ██   ██
██      ██    ██ ██ ████ ██ ██ ████ ██ ███████ ██ ██  ██ ██   ██
██      ██    ██ ██  ██  ██ ██  ██  ██ ██   ██ ██  ██ ██ ██   ██
 ██████  ██████  ██      ██ ██      ██ ██   ██ ██   ████ ██████ 
*/





int GetSettingIndexFromString(const char[] string)
{
    for (int setting = 0; setting < NUM_SETTINGS; setting++) {
        char cmdstub[128];
        Format(cmdstub, 128, "sm_jumpqol_%s", g_settings[setting].name);

        if (strncmp(string, g_settings[setting].name, strlen(string), false) == 0 || strncmp(string, cmdstub, strlen(cmdstub), false) == 0)
            return setting;
    }

    return -1;
}

void GetSettingErrorString(Setting setting, SettingError error, char[] error_string, int maxlength)
{
    if (error == SETTING_NO_ERROR) {
        Format(error_string, maxlength, "");
        return;
    }

    SettingType type = setting.type;
    if (type == SETTING_FLOAT)
        Format(error_string, maxlength, "The setting \"%s\" only accepts numbers from %.1f to %.1f.", setting.name, setting.range[0], setting.range[1]);
    else if (type == SETTING_INT)
        Format(error_string, maxlength, "The setting \"%s\" only accepts integers from %d to %d.", setting.name, RoundFloat(setting.range[0]), RoundFloat(setting.range[1]));
    else if (type == SETTING_BOOL)
        Format(error_string, maxlength, "The setting \"%s\" only accepts 0 or 1.", setting.name);
}

void GetSettingValueString(Setting setting, int client, char[] setting_value_string, int maxlength)
{
    SettingType type = setting.type;
    any value = setting.values[client];
    any value_pref = setting.prefs[client];
    any value_default = setting.GetDefault();


    char value_string[32] = "";
    if (type == SETTING_FLOAT)
        Format(value_string, 32, "%.1f", value);
    else if (type == SETTING_INT || type == SETTING_BOOL)
        Format(value_string, 32, "%d", value);

    char pref_string[32] = "";
    if (type == SETTING_FLOAT)
        Format(pref_string, 32, "pref. %.1f", value_pref);
    else if (type == SETTING_INT || type == SETTING_BOOL)
        Format(pref_string, 32, "pref. %d", value_pref);

    char default_string[32] = "";
    if (type == SETTING_FLOAT)
        Format(default_string, 32, "def. %.1f", value_default);
    else if (type == SETTING_INT || type == SETTING_BOOL)
        Format(default_string, 32, "def. %d", value_default);


    char info_string[32] = "";

    if (client != 0 && value != value_pref)
        Format(info_string, 32, "%s", pref_string);

    if (client == 0 || value != value_default) {
        if (!StrEqual(info_string, ""))
            Format(info_string, 32, "%s, ", info_string);

        Format(info_string, 32, "%s%s", info_string, default_string);
    }

    if (client == 0)
        Format(info_string, 32, "%s, enf. %d", info_string, setting.IsEnforced());

    if (!StrEqual(info_string, ""))
        Format(info_string, 32, " (%s)", info_string);


    if (client == 0)
        Format(setting_value_string, maxlength, "%s%s", setting.name, info_string);
    else
        Format(setting_value_string, maxlength, "%s = %s%s", setting.name, value_string, info_string);
}

void ForceDefault(Setting setting)
{
    for (int client = 1; client <= MaxClients; client++)
        if (IsClientInGame(client))
            setting.ChangeValue(client, setting.GetDefault(), true, true);
}

void ConVarChanged_Setting_Default(ConVar convar, const char[] old_value, const char[] new_value)
{
    char string_convar[128];
    convar.GetName(string_convar, 128);
    int index = GetSettingIndexFromString(string_convar);

    any value;
    SettingError error = g_settings[index].StringToValueEx(new_value, value);

    if (error != SETTING_NO_ERROR) {
        char error_string[128];
        GetSettingErrorString(g_settings[index], error, error_string, sizeof(error_string));
        PrintToServer(error_string);
        convar.SetString(old_value);
        return;
    }

    if (g_settings[index].IsEnforced())
        ForceDefault(g_settings[index]);
}

void ConVarChanged_Setting_Enforce(ConVar convar, const char[] old_value, const char[] new_value)
{
    char string_convar[128];
    convar.GetName(string_convar, 128);
    int index = GetSettingIndexFromString(string_convar);

    float value_float;
    bool isfloat = StringToFloatEx(new_value, value_float) > 0;

    int value_int;
    bool isint = isfloat && StringToIntEx(new_value, value_int) > 0;
    isint &= (RoundFloat(value_float) == value_int);

    bool isbool = isint && (0.0 <= value_float <= 1.0);

    if (!isbool) {
        PrintToServer("sm_jumpqol_%s_enforce only accepts 0 or 1", g_settings[index].name);
        convar.SetString(old_value);
        return;
    }

    if (g_settings[index].IsEnforced())
        ForceDefault(g_settings[index]);
}

void SetValueFromCommand(int client, Setting setting, const char[] value_string)
{
    if (client == 0) {
        ReplyToCommand(client, "Use sm_jumpqol_%s_default to set the default value.", setting.name);
        return;
    }

    if (setting.IsEnforced()) {
        ReplyToCommand(client, "Server doesn't allow \"%s\" to be changed.", setting.name);
        return;
    }

    any value;
    SettingError error = setting.StringToValueEx(value_string, value);

    if (error != SETTING_NO_ERROR) {
        char error_string[128];
        GetSettingErrorString(setting, error, error_string, sizeof(error_string));
        ReplyToCommand(client, error_string);
        return;
    }

    SettingAllow allow = setting.ChangeValue(client, value);

    if (allow != SETTING_ALLOW && StrEqual(g_error, ""))
        ReplyToCommand(client, g_error);
}

Action Command_Plugin(int client, int args)
{
    if (!IsDedicatedServer() && client == 0)
        client = 1;

    if (args == 0) {
        ReplyToCommand(client, "JumpQoL settings");

        ReplySource source = GetCmdReplySource();
        for (int setting = 0; setting < NUM_SETTINGS; setting++)
            if (g_settings[setting].working && (!g_settings[setting].IsEnforced() || source == SM_REPLY_TO_CONSOLE)) {
                char setting_value_string[64];
                GetSettingValueString(g_settings[setting], client, setting_value_string, 64);

                ReplyToCommand(client, "- %s", setting_value_string);
                ReplyToCommand(client, "  %s", g_settings[setting].desc);
            }

        return Plugin_Handled;
    }

    char setting_string[128];
    GetCmdArg(1, setting_string, 128);
    int index = GetSettingIndexFromString(setting_string);

    if (index == -1) {
        ReplyToCommand(client, "\"%s\" is not a valid setting.", setting_string);
        return Plugin_Handled;
    }

    if (!g_settings[index].working) {
        ReplyToCommand(client, "This setting is unavailable.");
        return Plugin_Handled;
    }

    if (args == 1) {
        char setting_value_string[64];
        GetSettingValueString(g_settings[index], client, setting_value_string, 64);

        ReplyToCommand(client, setting_value_string);
        ReplyToCommand(client, g_settings[index].desc);

        if (client != 0 && StrEqual(g_settings[index].name, "fakedelay"))
            ReplyToCommand(client, "Real delay is currently %d ms. Use this value to fake this server's delay on other servers.", RoundToNearest(GetDelay(client)*g_globals.interval_per_tick*1000.0));

        if (!StrEqual(g_settings[index].expl, ""))
            ReplyToCommand(client, g_settings[index].expl);

        return Plugin_Handled;
    }

    char value_string[128];
    GetCmdArg(2, value_string, 128);

    SetValueFromCommand(client, g_settings[index], value_string);

    return Plugin_Handled;
}

Action Command_Setting(int client, int args)
{
    if (!IsDedicatedServer() && client == 0)
        client = 1;

    char cmd[128];
    GetCmdArg(0, cmd, 128);
    int index = GetSettingIndexFromString(cmd);

    if (!g_settings[index].working) {
        ReplyToCommand(client, "This setting is unavailable.");
        return Plugin_Handled;
    }

    if (args == 0) {
        char setting_value_string[64];
        GetSettingValueString(g_settings[index], client, setting_value_string, 64);

        ReplyToCommand(client, setting_value_string);
        ReplyToCommand(client, g_settings[index].desc);

        if (client != 0 && StrEqual(g_settings[index].name, "fakedelay"))
            ReplyToCommand(client, "Real delay is currently %d ms. Use this value to fake this server's delay on other servers.", RoundToNearest(GetDelay(client)*g_globals.interval_per_tick*1000.0));

        if (!StrEqual(g_settings[index].expl, ""))
            ReplyToCommand(client, g_settings[index].expl);

        return Plugin_Handled;
    }

    char value_string[128];
    GetCmdArg(1, value_string, 128);

    SetValueFromCommand(client, g_settings[index], value_string);

    return Plugin_Handled;
}





/*
_required
██████  ███████  ██████  ██    ██ ██ ██████  ███████ ██████ 
██   ██ ██      ██    ██ ██    ██ ██ ██   ██ ██      ██   ██
██████  █████   ██    ██ ██    ██ ██ ██████  █████   ██   ██
██   ██ ██      ██ ▄▄ ██ ██    ██ ██ ██   ██ ██      ██   ██
██   ██ ███████  ██████   ██████  ██ ██   ██ ███████ ██████ 
                    ▀▀                                      
*/





ConVar g_Required_ConVar_maxevents;
Handle g_Required_Call_CBaseFilter__PassesFilter;
bool Required_Init()
{
    g_globals = Globals(GameConfGetAddress(g_gameconf, "gpGlobals"));
    if (!g_globals)
        return SetError("Failed to find gpGlobals.");

    if (!g_detours[DETOUR_PHYSICS_SIMULATEENTITY].Enable(Required_Detour_Pre_Physics_SimulateEntity, Required_Detour_Post_Physics_SimulateEntity))
        return false;

    if (!g_detours[DETOUR_SERVICEEVENTS].Enable(Required_Detour_Pre_CEventQueue__ServiceEvents, _))
        return false;

    g_Required_ConVar_maxevents = CreateConVar(
        "sm_jumpqol_required_maxevents",
        "-1",
        "Max amount of game events that should be checked in a single frame to support gimmicks.\n-1:No max.\n0:Disable checks.\n1 or above: Max amount.",
        _,
        true,
        -1.0
    );

    if (!g_detours[DETOUR_ITEMPOSTFRAME].Enable(Required_Detour_Pre_CTFWeaponBase__ItemPostFrame, Required_Detour_Post_CTFWeaponBase__ItemPostFrame))
        return false;

    StartPrepSDKCall(SDKCall_Entity);
    if (!PrepSDKCall_SetFromConf(g_gameconf, SDKConf_Signature, "CBaseFilter::PassesFilter"))
        return SetError("Failed to prepare CBaseFilter::PassesFilter call.");

    PrepSDKCall_AddParameter(SDKType_CBaseEntity, SDKPass_Pointer); // pCaller (trigger)
    PrepSDKCall_AddParameter(SDKType_CBaseEntity, SDKPass_Pointer); // pEntity (projectile)

    PrepSDKCall_SetReturnInfo(SDKType_Bool, SDKPass_Plain);

    g_Required_Call_CBaseFilter__PassesFilter = EndPrepSDKCall();
    if (g_Required_Call_CBaseFilter__PassesFilter == INVALID_HANDLE)
        return SetError("Failed to prepare CBaseFilter::PassesFilter call.");

    return true;
}





public void OnGameFrame()
{
    g_curtime_frame = g_globals.curtime;
    g_frametime_frame = g_globals.frametime;
    g_tickcount_frame = g_globals.tickcount;

    for (int client = 1; client <= MaxClients; client++)
        for (int i = 0; i < g_numprojs[client]; i++)
            g_projs[client][i].updated = false;
}





int g_created = -1;
public void OnEntityCreated(int entity, const char[] classname)
{
    // if (strncmp(classname, "tf_projectile_", 14) != 0)
    //     return;

    if (   !StrEqual(classname, "tf_projectile_rocket")
        && !StrEqual(classname, "tf_projectile_energy_ball")
        && !StrEqual(classname, "tf_projectile_sentryrocket")
    )
        return;

    g_created = entity;
}

public void OnEntityDestroyed(int entity)
{
    if (entity < 0)
        return;

    ProjectileInfo proj; proj = FindProjectile(entity);
    if (IsActivePlayer(proj.client))
        g_delprojs[proj.client][g_numdelprojs[proj.client]++] = proj.ref;

    g_findclients[entity] = -1;
}





MRESReturn Required_Detour_Pre_Physics_SimulateEntity(DHookParam hParams)
{
    int entity = DHookGetParam(hParams, 1);
    if (!IsValidEdict(entity))
        return MRES_Ignored;

    g_entity_simulating = entity;
    if (IsActivePlayer(entity))
        g_player_simulating = entity;

    return MRES_Ignored;
}

MRESReturn Required_Detour_Post_Physics_SimulateEntity(DHookParam hParams)
{
    int entity = DHookGetParam(hParams, 1);
    if (!IsValidEdict(entity))
        return MRES_Ignored;

    g_entity_simulating = -1;
    if (IsActivePlayer(entity))
        g_player_simulating = -1;

    ProjectileInfo proj; proj = FindProjectile(entity);
    if (!IsActivePlayer(proj.client))
        return MRES_Ignored;

    // Can't think of a good way to tell it there was no update because of sync (maybe run this in a CBaseEntity::PhysicsSimulate detour instead?)
    bool predictable = g_projs[proj.client][proj.index].IsPredictable();
    bool sync = g_sessions[proj.client].sync;
    if (!sync || !predictable || (sync && g_allow_update))
        g_projs[proj.client][proj.index].Update(sync && g_allow_update);

    #if DEBUG
    if (g_debug_output != -1 && proj.client == g_debug_target && debug_toggles[DEBUG_UPDATE]) {
        Projectile projstruct;
        projstruct = g_projs[proj.client][proj.index];
        PrintToConsole(g_debug_output, "update %d - proj: %d, movetype: %s, basevel: %s, manipulated: %s, last manip frame %d, current update frame: %d", g_tickcount_frame, entity, projstruct.IsMoveTypeSupported() ? "supported" : "unsupported", projstruct.HasBaseVelocity() ? "yes" : "no", projstruct.IsManipulated() ? "yes" : "no", projstruct.frame_manipulated, projstruct.frame);
    }
    #endif

    float pos[3];
    float vel[3];
    GetEntPropVector(entity, Prop_Data, "m_vecAbsOrigin", pos);
    GetEntPropVector(entity, Prop_Data, "m_vecAbsVelocity", vel);
    TR_EnumerateEntitiesSphere(pos, 5.0*GetVectorLength(vel)*g_globals.interval_per_tick, PARTITION_TRIGGER_EDICTS, Required_CheckTriggerFilter, entity);

    return MRES_Ignored;
}

bool Required_CheckTriggerFilter(int trigger, any entity)
{
    char classname[64];
    GetEntityClassname(trigger, classname, sizeof(classname));

    if (strncmp(classname, "trigger_", 8) != 0)
        return true;

    int filter = GetEntPropEnt(trigger, Prop_Data, "m_hFilter");
    bool negated = filter != -1 ? view_as<bool>(GetEntProp(filter, Prop_Data, "m_bNegated", 1)) : false;

    bool filters = false;
    if (filter != -1 && !negated)
        filters = SDKCall(g_Required_Call_CBaseFilter__PassesFilter, filter, trigger, entity);

    if (filters) {
        ProjectileInfo proj; proj = FindProjectile(entity);

        #if DEBUG
        if (g_debug_output != -1 && proj.client == g_debug_target && debug_toggles[DEBUG_MANIP]) {
            PrintToConsole(g_debug_output, "manip %d - trigger - proj: %d, trigger: %d, classname: %s", g_tickcount_frame, entity, trigger, classname);
        }
        #endif

        if (g_projs[proj.client][proj.index].frame_manipulated < g_tickcount_frame)
            g_projs[proj.client][proj.index].frame_manipulated = g_tickcount_frame;
    }

    return true;
}





methodmap EventQueueEvent
{
    public EventQueueEvent(Address address)
    {
        return view_as<EventQueueEvent>(address);
    }

    property float m_flFireTime
    {
        public get()
        {
            return view_as<float>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(0), NumberType_Int32) );
        }
    }

    property Address m_iTarget
    {
        public get()
        {
            return view_as<Address>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(4), NumberType_Int32) );
        }
    }

    property Address m_iTargetInput
    {
        public get()
        {
            return view_as<Address>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(8), NumberType_Int32) );
        }
    }

    property int m_pActivator
    {
        public get()
        {
            return view_as<int>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(12), NumberType_Int32) );
        }
    }

    property int m_pCaller
    {
        public get()
        {
            return view_as<int>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(16), NumberType_Int32) );
        }
    }

    property EventQueueEvent m_pNext
    {
        public get()
        {
            return view_as<EventQueueEvent>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(48), NumberType_Int32) );
        }
    }
}

MRESReturn Required_Detour_Pre_CEventQueue__ServiceEvents(Address pThis)
{
    int maxevents = g_Required_ConVar_maxevents.IntValue;
    if (maxevents == 0)
        return MRES_Ignored;

    int num = 0;
    char targets[MAXPLAYERS*MAX_PROJECTILES][256];
    int lengths[MAXPLAYERS*MAX_PROJECTILES];
    int firetimes[MAXPLAYERS*MAX_PROJECTILES];

    EventQueueEvent event = view_as<EventQueueEvent>(pThis + view_as<Address>(0));
    while (view_as<Address>(event) != Address_Null) {
        if (event.m_iTarget == Address_Null) {
            event = event.m_pNext;
            continue;
        }

        LoadFromStringAddress(event.m_iTarget, targets[num]);
        if (StrEqual(targets[num], "")) {
            event = event.m_pNext;
            continue;
        }

        lengths[num] = strlen(targets[num]);
        firetimes[num] = RoundToCeil(event.m_flFireTime / g_globals.interval_per_tick);

        num++;
        if (maxevents != -1 && num >= maxevents)
            break;

        event = event.m_pNext;
    }

    if (num == 0)
        return MRES_Ignored;

    for (int client = 1; client <= MaxClients; client++) {
        if (!IsActivePlayer(client))
            continue;

        for (int i = 0; i < g_numprojs[client]; i++) {
            int entity = g_projs[client][i].Entity();
            if (entity == -1)
                continue;

            char name[256];
            GetEntPropString(entity, Prop_Data, "m_iName", name, sizeof(name));
            if (StrEqual(name, ""))
                return MRES_Ignored;

            for (int j = 0; j < num; j++) {
                if (targets[j][lengths[j] - 1] == '*') {
                    if (strncmp(targets[j], name, lengths[j] - 1, false) != 0)
                        continue;
                }
                else {
                    if (strcmp(targets[j], name, false) != 0)
                        continue;
                }

                #if DEBUG
                if (g_debug_output != -1 && client == g_debug_target && debug_toggles[DEBUG_MANIP]) {
                    char targetinput[256];
                    LoadFromStringAddress(event.m_iTargetInput, targetinput);
                    PrintToConsole(g_debug_output, "manip %d - event - proj: %d, m_iTarget: %s, m_iTargetInput: %s, m_pActivator %d, m_pCaller %d", g_tickcount_frame, g_projs[client][i].Entity(), targets[j], targetinput, event.m_pActivator, event.m_pCaller);
                }
                #endif

                if (g_projs[client][i].frame_manipulated < firetimes[j])
                    g_projs[client][i].frame_manipulated = firetimes[j];
            }
        }
    }

    return MRES_Ignored;
}





public void OnPlayerRunCmdPre(int client, int buttons, int impulse, const float vel[3], const float angles[3], int weapon, int subtype, int cmdnum, int tickcount, int seed, const int mouse[2])
{
    g_inside_cmd = true;
}

public void OnPlayerRunCmdPost(int client, int buttons, int impulse, const float vel[3], const float angles[3], int weapon, int subtype, int cmdnum, int tickcount, int seed, const int mouse[2])
{
    g_inside_cmd = false;

    // Clear any removed projectiles from list
    for (int i = 0; i < g_numdelprojs[client]; i++)
        g_sessions[client].RemoveProjectile(g_delprojs[client][i]);
    g_numdelprojs[client] = 0;

    if (g_settings[SETTING_SYNC].active[client]) {
        Sync_OnPlayerRunCmdPost(client);

        // Clear any removed projectiles from list
        for (int i = 0; i < g_numdelprojs[client]; i++)
            g_sessions[client].RemoveProjectile(g_delprojs[client][i]);
        g_numdelprojs[client] = 0;
    }

    // Velocity and other stuff have been set at this point
    for (int i = 0; i < g_numnewprojs[client]; i++)
        g_sessions[client].AddProjectile(g_newprojs[client][i]);
    g_numnewprojs[client] = 0;
}

public void OnSendClientMessages()
{
    if (g_settings[SETTING_SYNC].numusing != 0)
        Sync_OnSendClientMessages();

    if (g_settings[SETTING_FAKEDELAY].numusing != 0)
        Fakedelay_OnSendClientMessages();
}





MRESReturn Required_Detour_Pre_CTFWeaponBase__ItemPostFrame(int entity)
{
    g_weapon_simulating = entity;
    g_created = -1;

    return MRES_Handled;
}

MRESReturn Required_Detour_Post_CTFWeaponBase__ItemPostFrame(int entity)
{
    int created = g_created;

    g_weapon_simulating = -1;
    g_created = -1;

    if (!IsValidEdict(created))
        return MRES_Ignored;

    int owner = GetOwner(entity);

    if (!IsActivePlayer(owner))
        return MRES_Ignored;

    if (g_numnewprojs[owner] < MAX_PROJECTILES) {
        g_findclients[created] = owner;
        g_newprojs[owner][g_numnewprojs[owner]++] = EntIndexToEntRef(created);
    }

    return MRES_Ignored;
}





/*
_projdecals
██████  ██████   ██████       ██ ██████  ███████  ██████  █████  ██      ███████
██   ██ ██   ██ ██    ██      ██ ██   ██ ██      ██      ██   ██ ██      ██     
██████  ██████  ██    ██      ██ ██   ██ █████   ██      ███████ ██      ███████
██      ██   ██ ██    ██ ██   ██ ██   ██ ██      ██      ██   ██ ██           ██
██      ██   ██  ██████   █████  ██████  ███████  ██████ ██   ██ ███████ ███████
*/





bool Projdecals_Start()
{
    if (!g_detours[DETOUR_UTIL_DECALTRACE].Enable(Projdecals_Detour_Pre_UTIL_DecalTrace, Projdecals_Detour_Post_UTIL_DecalTrace))
        return false;

    return true;
}

void Projdecals_Stop()
{
    g_detours[DETOUR_UTIL_DECALTRACE].Disable(Projdecals_Detour_Pre_UTIL_DecalTrace, Projdecals_Detour_Post_UTIL_DecalTrace);
}

float g_startpos[3];
MRESReturn Projdecals_Detour_Pre_UTIL_DecalTrace(DHookParam hParams)
{
    g_startpos = NaNVector;

    int client = -1;
    if (g_weapon_simulating != -1)
        client = g_player_simulating;
    else if (g_entity_simulating != -1)
        client = FindProjectile(g_entity_simulating).client;

    if (!IsActivePlayer(client))
        return MRES_Ignored;

    if (g_sessions[client].projdecals)
        return MRES_Ignored;

    Trace trace = Trace(DHookGetParam(hParams, 1));
    g_startpos = LoadFromVectorAddress(trace.startpos);

    float endpos[3];
    endpos = LoadFromVectorAddress(trace.endpos);

    float normal[3];
    normal = LoadFromVectorAddress(trace.plane.normal);

    float temp[3];
    SubtractVectors(g_startpos, endpos, temp);
    float dist = DotVectors(temp, normal);

    OffsetVector(endpos, normal, dist, temp);

    StoreToVectorAddress(temp, trace.startpos);

    return MRES_Ignored;
}

MRESReturn Projdecals_Detour_Post_UTIL_DecalTrace(DHookParam hParams)
{
    if (VectorIsNaN(g_startpos))
        return MRES_Ignored;

    Trace trace = view_as<Trace>(DHookGetParam(hParams, 1));
    StoreToVectorAddress(g_startpos, trace.startpos);

    return MRES_Ignored;
}





/*
_showdetdecals
███████ ██   ██  ██████  ██     ██ ██████  ███████ ████████ ██████  ███████  ██████  █████  ██      ███████
██      ██   ██ ██    ██ ██     ██ ██   ██ ██         ██    ██   ██ ██      ██      ██   ██ ██      ██     
███████ ███████ ██    ██ ██  █  ██ ██   ██ █████      ██    ██   ██ █████   ██      ███████ ██      ███████
     ██ ██   ██ ██    ██ ██ ███ ██ ██   ██ ██         ██    ██   ██ ██      ██      ██   ██ ██           ██
███████ ██   ██  ██████   ███ ███  ██████  ███████    ██    ██████  ███████  ██████ ██   ██ ███████ ███████
*/





IPredictionSystem g_showdetdecals_te;
bool Showdetdecals_Init()
{
    g_showdetdecals_te = IPredictionSystem(GameConfGetAddress(g_gameconf, "te"));
    if (!g_showdetdecals_te)
        return SetError("Failed to find te.");

    return true;
}

bool Showdetdecals_Start()
{
    if (!g_detours[DETOUR_UTIL_DECALTRACE].Enable(Showdetdecals_Detour_Pre_UTIL_DecalTrace, Showdetdecals_Detour_Post_UTIL_DecalTrace))
        return false;

    return true;
}

void Showdetdecals_Stop()
{
    g_detours[DETOUR_UTIL_DECALTRACE].Disable(Showdetdecals_Detour_Pre_UTIL_DecalTrace, Showdetdecals_Detour_Post_UTIL_DecalTrace);
}

Address g_showdetdecals_suppresshost = Address_Null;
MRESReturn Showdetdecals_Detour_Pre_UTIL_DecalTrace(DHookParam hParams)
{
    g_showdetdecals_suppresshost = g_showdetdecals_te.m_pSuppressHost;

    if (g_weapon_simulating == -1)
        return MRES_Ignored;

    int client = g_player_simulating;
    if (!IsActivePlayer(client))
        return MRES_Ignored;

    if (!g_sessions[client].showdetdecals)
        return MRES_Ignored;

    g_showdetdecals_te.m_pSuppressHost = Address_Null;

    return MRES_Ignored;
}

MRESReturn Showdetdecals_Detour_Post_UTIL_DecalTrace(DHookParam hParams)
{
    g_showdetdecals_te.m_pSuppressHost = g_showdetdecals_suppresshost;

    return MRES_Ignored;
}





/*
_keepblaststate
██   ██ ███████ ███████ ██████  ██████  ██       █████  ███████ ████████ ███████ ████████  █████  ████████ ███████
██  ██  ██      ██      ██   ██ ██   ██ ██      ██   ██ ██         ██    ██         ██    ██   ██    ██    ██     
█████   █████   █████   ██████  ██████  ██      ███████ ███████    ██    ███████    ██    ███████    ██    █████  
██  ██  ██      ██      ██      ██   ██ ██      ██   ██      ██    ██         ██    ██    ██   ██    ██    ██     
██   ██ ███████ ███████ ██      ██████  ███████ ██   ██ ███████    ██    ███████    ██    ██   ██    ██    ███████
*/





bool Keepblaststate_Start()
{
    if (!g_detours[DETOUR_TFPLAYERTHINK].Enable(Keepblaststate_Detour_Pre_CTFPlayer__TFPlayerThink, Keepblaststate_Detour_Post_CTFPlayer__TFPlayerThink))
        return false;

    return true;
}

void Keepblaststate_Stop()
{
    g_detours[DETOUR_TFPLAYERTHINK].Disable(Keepblaststate_Detour_Pre_CTFPlayer__TFPlayerThink, Keepblaststate_Detour_Post_CTFPlayer__TFPlayerThink);
}

int g_keepblaststate_groundentity = -1;
MRESReturn Keepblaststate_Detour_Pre_CTFPlayer__TFPlayerThink(int client)
{
    if (!g_sessions[client].keepblaststate)
        return MRES_Ignored;

    g_keepblaststate_groundentity = GetEntPropEnt(client, Prop_Data, "m_hGroundEntity");

    // Temporarily set the ground entity to null
    if (g_keepblaststate_groundentity != -1) {
        float vel[3];
        GetEntPropVector(client, Prop_Data, "m_vecAbsVelocity", vel);

        if (vel[2] > 250.0)
            SetEntPropEnt(client, Prop_Data, "m_hGroundEntity", -1);
    }

    return MRES_Ignored;
}

MRESReturn Keepblaststate_Detour_Post_CTFPlayer__TFPlayerThink(int client)
{
    if (!g_sessions[client].keepblaststate)
        return MRES_Ignored;

    if (g_keepblaststate_groundentity != -1)
        SetEntPropEnt(client, Prop_Data, "m_hGroundEntity", g_keepblaststate_groundentity);

    return MRES_Ignored;
}





/*
_reloadfire
██████  ███████ ██       ██████   █████  ██████  ███████ ██ ██████  ███████
██   ██ ██      ██      ██    ██ ██   ██ ██   ██ ██      ██ ██   ██ ██     
██████  █████   ██      ██    ██ ███████ ██   ██ █████   ██ ██████  █████  
██   ██ ██      ██      ██    ██ ██   ██ ██   ██ ██      ██ ██   ██ ██     
██   ██ ███████ ███████  ██████  ██   ██ ██████  ██      ██ ██   ██ ███████
*/





bool g_reloadfire_client_working;
bool LogReloadFireSoundFixWarning(const char[] message)
{
    g_reloadfire_client_working = false;
    LogMessage("Unable to setup reloadfire sound fix: %s. Are you using SourceMod 1.12?", message);
    return true;
}





Handle g_Call_ItemPostFrame;
Handle g_Call_Client_ItemPostFrame;
int g_reloadfire_client_m_RefEHandle_offset;
int g_reloadfire_client_m_flNextAttack_offset;
bool Reloadfire_Init()
{
    StartPrepSDKCall(SDKCall_Entity);
    if (!PrepSDKCall_SetFromConf(g_gameconf, SDKConf_Virtual, "CBaseCombatWeapon::ItemPostFrame"))
        return SetError("Failed to prepare CBaseCombatWeapon::ItemPostFrame call.");

    g_Call_ItemPostFrame = EndPrepSDKCall();
    if (g_Call_ItemPostFrame == INVALID_HANDLE)
        return SetError("Failed to prepare CBaseCombatWeapon::ItemPostFrame call.");

    if (IsDedicatedServer())
        return true;

    // client only

    g_reloadfire_client_working = true;

    StartPrepSDKCall(SDKCall_Raw);
    if (!PrepSDKCall_SetFromConf(g_gameconf, SDKConf_Virtual, "client CBaseCombatWeapon::ItemPostFrame"))
        return LogReloadFireSoundFixWarning("Failed to prepare client CBaseCombatWeapon::ItemPostFrame call.");

    g_Call_Client_ItemPostFrame = EndPrepSDKCall();
    if (g_Call_Client_ItemPostFrame == INVALID_HANDLE)
        return LogReloadFireSoundFixWarning("Failed to prepare client CBaseCombatWeapon::ItemPostFrame call.");

    g_reloadfire_client_m_RefEHandle_offset = GameConfGetOffset(g_gameconf, "client CBaseEntity::m_RefEHandle");
    if (g_reloadfire_client_m_RefEHandle_offset == -1)
        return LogReloadFireSoundFixWarning("Failed to get client CBaseEntity::m_RefEHandle offset.");

    g_reloadfire_client_m_flNextAttack_offset = GameConfGetOffset(g_gameconf, "client CBaseCombatCharacter::m_flNextAttack");
    if (g_reloadfire_client_m_flNextAttack_offset == -1)
        return LogReloadFireSoundFixWarning("Failed to get client CBaseCombatCharacter::m_flNextAttack offset.");

    return true;
}

bool Reloadfire_Start()
{
    if (!g_detours[DETOUR_ITEMBUSYFRAME].Enable(_, Reloadfire_Detour_Post_CTFWeaponBase__ItemBusyFrame))
        return false;

    if (IsDedicatedServer() || !g_reloadfire_client_working)
        return true;

    // client only

    if (!g_detours[DETOUR_CLIENT_ITEMPOSTFRAME].Enable(Reloadfire_Detour_Client_Pre_CBasePlayer__ItemPostFrame, _))
        return LogReloadFireSoundFixWarning(g_error);

    if (!g_detours[DETOUR_CLIENT_ITEMBUSYFRAME].Enable(_, Reloadfire_Detour_Client_Post_CTFWeaponBase__ItemBusyFrame))
        return LogReloadFireSoundFixWarning(g_error);

    return true;
}

void Reloadfire_Stop()
{
    g_detours[DETOUR_ITEMBUSYFRAME].Disable(_, Reloadfire_Detour_Post_CTFWeaponBase__ItemBusyFrame);

    if (IsDedicatedServer() || !g_reloadfire_client_working)
        return;

    // client only

    g_detours[DETOUR_CLIENT_ITEMPOSTFRAME].Disable(Reloadfire_Detour_Client_Pre_CBasePlayer__ItemPostFrame, _);
    g_detours[DETOUR_CLIENT_ITEMBUSYFRAME].Disable(_, Reloadfire_Detour_Client_Post_CTFWeaponBase__ItemBusyFrame);
}

MRESReturn Reloadfire_Detour_Post_CTFWeaponBase__ItemBusyFrame(int entity)
{
    int client = GetEntPropEnt(entity, Prop_Data, "m_hOwner");
    if (!IsActivePlayer(client))
        return MRES_Ignored;

    if (!g_sessions[client].reloadfire)
        return MRES_Ignored;

    float m_flNextAttack = GetEntPropFloat(client, Prop_Data, "m_flNextAttack");
    if (m_flNextAttack < g_globals.curtime)
        return MRES_Ignored;

    SDKCall(g_Call_ItemPostFrame, entity);

    return MRES_Handled;
}





Address g_reloadfire_client_client;
MRESReturn Reloadfire_Detour_Client_Pre_CBasePlayer__ItemPostFrame(Address client)
{
    g_reloadfire_client_client = client;

    return MRES_Ignored;
}

MRESReturn Reloadfire_Detour_Client_Post_CTFWeaponBase__ItemBusyFrame(Address entity)
{
    int clienthandle = LoadFromAddress(g_reloadfire_client_client + view_as<Address>(g_reloadfire_client_m_RefEHandle_offset), NumberType_Int32);
    int client = clienthandle & (MAX_EDICTS - 1);
    if (!IsActivePlayer(client))
        return MRES_Ignored;

    if (!g_sessions[client].reloadfire)
        return MRES_Ignored;

    float m_flNextAttack = LoadFromAddress(g_reloadfire_client_client + view_as<Address>(g_reloadfire_client_m_flNextAttack_offset), NumberType_Int32);
    if (m_flNextAttack < g_globals.curtime)
        return MRES_Ignored;

    SDKCall(g_Call_Client_ItemPostFrame, entity);

    return MRES_Handled;
}





/*
_attack2fire
 █████  ████████ ████████  █████   ██████ ██   ██ ██████  ███████ ██ ██████  ███████
██   ██    ██       ██    ██   ██ ██      ██  ██       ██ ██      ██ ██   ██ ██     
███████    ██       ██    ███████ ██      █████    █████  █████   ██ ██████  █████  
██   ██    ██       ██    ██   ██ ██      ██  ██  ██      ██      ██ ██   ██ ██     
██   ██    ██       ██    ██   ██  ██████ ██   ██ ███████ ██      ██ ██   ██ ███████
*/





Handle g_Call_Energy_FullyCharged;
bool Attack2fire_Init()
{
    StartPrepSDKCall(SDKCall_Entity);
    if (!PrepSDKCall_SetFromConf(g_gameconf, SDKConf_Signature, "CTFWeaponBase::Energy_FullyCharged"))
        return SetError("Failed to prepare CTFWeaponBase::Energy_FullyCharged call.");

    PrepSDKCall_SetReturnInfo(SDKType_Bool, SDKPass_ByValue);

    g_Call_Energy_FullyCharged = EndPrepSDKCall();
    if (g_Call_Energy_FullyCharged == INVALID_HANDLE)
        return SetError("Failed to prepare CTFWeaponBase::Energy_FullyCharged call.");

    return true;
}

bool Attack2fire_Start()
{
    if (!g_detours[DETOUR_ITEMPOSTFRAME].Enable(Attack2fire_Detour_Pre_CTFWeaponBase__ItemPostFrame, _))
        return false;

    return true;
}

void Attack2fire_Stop()
{
    g_detours[DETOUR_ITEMPOSTFRAME].Disable(Attack2fire_Detour_Pre_CTFWeaponBase__ItemPostFrame, _);
}

MRESReturn Attack2fire_Detour_Pre_CTFWeaponBase__ItemPostFrame(int entity)
{
    int client = GetEntPropEnt(entity, Prop_Data, "m_hOwner");
    if (!IsActivePlayer(client))
        return MRES_Ignored;

    if (g_sessions[client].attack2fire == 0)
        return MRES_Ignored;

    char classname[64];
    GetEntityClassname(entity, classname, 64);

    if (   !StrEqual(classname, "tf_weapon_rocketlauncher")
        && !StrEqual(classname, "tf_weapon_rocketlauncher_directhit")
        && !StrEqual(classname, "tf_weapon_rocketlauncher_airstrike")
        && !StrEqual(classname, "tf_weapon_particle_cannon")
    )
        return MRES_Ignored;

    bool cancharge = StrEqual(classname, "tf_weapon_particle_cannon") ? SDKCall(g_Call_Energy_FullyCharged, entity) : false;
    if (GetClientButtons(client) & IN_ATTACK)
        cancharge = false;

    if (cancharge && g_sessions[client].attack2fire == 1 && (GetClientButtons(client) & IN_ATTACK2) && !g_chargemsg[client]) {
        PrintToChat(client, "\x01Use \x074FDDFF/jumpqol attack2fire 2\x01 to allow Cow Mangler 5000 charged shots.");
        g_chargemsg[client] = true;
    }

    if (GetEntProp(entity, Prop_Data, "m_iClip1") == 0 || cancharge && g_sessions[client].attack2fire == 2)
        SetEntPropFloat(entity, Prop_Send, "m_flNextSecondaryAttack", g_globals.curtime - g_globals.interval_per_tick);
    else
        SetEntPropFloat(entity, Prop_Send, "m_flNextSecondaryAttack", g_globals.curtime + 0.5);

    return MRES_Handled;
}





/*
_rampfix
██████   █████  ███    ███ ██████  ███████ ██ ██   ██
██   ██ ██   ██ ████  ████ ██   ██ ██      ██  ██ ██ 
██████  ███████ ██ ████ ██ ██████  █████   ██   ███  
██   ██ ██   ██ ██  ██  ██ ██      ██      ██  ██ ██ 
██   ██ ██   ██ ██      ██ ██      ██      ██ ██   ██
*/





bool Rampfix_Start()
{
    if (!g_detours[DETOUR_SETGROUNDENTITY].Enable(Rampfix_Detour_Pre_CTFGameMovement__SetGroundEntity, _))
        return false;

    return true;
}

void Rampfix_Stop()
{
    g_detours[DETOUR_SETGROUNDENTITY].Disable(Rampfix_Detour_Pre_CTFGameMovement__SetGroundEntity, _);
}

MRESReturn Rampfix_Detour_Pre_CTFGameMovement__SetGroundEntity(Address pThis, DHookParam hParams)
{
    Address player_address = view_as<Address>(LoadFromAddress(pThis + view_as<Address>(4), NumberType_Int32));
    Address movedata_address = view_as<Address>(LoadFromAddress(pThis + view_as<Address>(8), NumberType_Int32));

    if (DHookIsNullParam(hParams, 1))
        return MRES_Ignored;

    int client = -1;
    for (int i = 1; i <= MaxClients; i++) {
        if (IsClientInGame(i) && GetEntityAddress(i) == player_address) {
            client = i;
            break;
        }
    }

    if (!IsActivePlayer(client))
        return MRES_Ignored;

    if (!g_sessions[client].rampfix)
        return MRES_Ignored;


    float gravity = GetGravity(client);

    Trace trace_ramp = Trace(DHookGetParamAddress(hParams, 1));

    // Current velocity
    float vel_move[3];
    vel_move = LoadFromVectorAddress(movedata_address + view_as<Address>(68));

    // Use what the velocity would have been on the next tick
    float vel_next[3];
    CopyVector(vel_move, vel_next);
    vel_next[2] -= gravity * g_globals.frametime;

    float normal_ramp[3];
    normal_ramp = LoadFromVectorAddress(trace_ramp.plane.normal);

    float incident_ramp = GetVectorDotProduct(vel_next, normal_ramp);

    // Velocity on ramp next tick
    float vel_ramp[3];
    OffsetVector(vel_next, normal_ramp, -incident_ramp, vel_ramp);

    if (incident_ramp < 0.0 && vel_ramp[2] > 250.0)
        return MRES_Supercede;

    return MRES_Ignored;
}





/*
_slideebfix
███████ ██      ██ ██████  ███████ ███████ ██████  ███████ ██ ██   ██
██      ██      ██ ██   ██ ██      ██      ██   ██ ██      ██  ██ ██ 
███████ ██      ██ ██   ██ █████   █████   ██████  █████   ██   ███  
     ██ ██      ██ ██   ██ ██      ██      ██   ██ ██      ██  ██ ██ 
███████ ███████ ██ ██████  ███████ ███████ ██████  ██      ██ ██   ██
*/





Address g_slideebfix_movehelper;
bool Slideebfix_Init()
{
    g_slideebfix_movehelper = GameConfGetAddress(g_gameconf, "&MoveHelperServer()::s_MoveHelperServer");
    if (!g_slideebfix_movehelper)
        return SetError("Failed to find &MoveHelperServer()::s_MoveHelperServer.");

    return true;
}

bool Slideebfix_Start()
{
    if (!g_detours[DETOUR_SETGROUNDENTITY].Enable(Slideebfix_Detour_Pre_CTFGameMovement__SetGroundEntity, _))
        return false;

    return true;
}

void Slideebfix_Stop()
{
    g_detours[DETOUR_SETGROUNDENTITY].Disable(Slideebfix_Detour_Pre_CTFGameMovement__SetGroundEntity, _);
}

#define GROUND_LAND_INTERVAL (2.0)
bool TraceEntityFilterPlayer(int entity, int contentsMask) { return (entity == 0 || entity > MaxClients); }
MRESReturn Slideebfix_Detour_Pre_CTFGameMovement__SetGroundEntity(Address pThis, DHookParam hParams)
{
    Address player_address = view_as<Address>(LoadFromAddress(pThis + view_as<Address>(4), NumberType_Int32));
    Address movedata_address = view_as<Address>(LoadFromAddress(pThis + view_as<Address>(8), NumberType_Int32));

    if (DHookIsNullParam(hParams, 1))
        return MRES_Ignored;

    int client = -1;
    for (int i = 1; i <= MaxClients; i++) {
        if (IsClientInGame(i) && GetEntityAddress(i) == player_address) {
            client = i;
            break;
        }
    }

    if (!IsActivePlayer(client))
        return MRES_Ignored;

    if (!g_sessions[client].rampfix)
        return MRES_Ignored;


    float gravity = GetGravity(client);


    int m_TouchList_m_Size = LoadFromAddress(g_slideebfix_movehelper + view_as<Address>(8) + view_as<Address>(12), NumberType_Int32);
    if (m_TouchList_m_Size == 0)
        return MRES_Ignored; // No collisions during this tick

    Address m_TouchList_m_pElements = LoadFromAddress(g_slideebfix_movehelper + view_as<Address>(8) + view_as<Address>(16), NumberType_Int32);

    Trace trace_slide = Trace(m_TouchList_m_pElements + view_as<Address>(0*96) + view_as<Address>(12));
    Trace trace_bottom = Trace(DHookGetParamAddress(hParams, 1));

    // Velocity before movement and collisions
    float vel_before[3];
    vel_before = LoadFromVectorAddress(m_TouchList_m_pElements + view_as<Address>(0*96) + view_as<Address>(0));

    // Slide normal
    float normal_slide[3];
    normal_slide = LoadFromVectorAddress(trace_slide.plane.normal);

    // Horizontal normal of the slide
    float normal_slide_h[3];
    CopyVector(normal_slide, normal_slide_h);
    normal_slide_h[2] = 0.0;
    NormalizeVector(normal_slide_h, normal_slide_h);

    // Horizontal speed down the slide
    float vel_before_h = GetVectorDotProduct(vel_before, normal_slide_h);

    // Only pass if the player was sliding down something steep before hitting ground...
    if (normal_slide[2] >= 0.7 || vel_before[2] >= 0.0 || vel_before_h < 30.0)
        return MRES_Ignored;

    // Velocity at the end of the previous tick
    float vel_prev[3];
    GetEntPropVector(client, Prop_Data, "m_vecAbsVelocity", vel_prev);
    vel_prev[2] += 0.5 * gravity * g_globals.frametime; // Only half gravity from previous tick

    // ...and it is something they where already sliding on
    if (FloatAbs(GetVectorDotProduct(vel_prev, normal_slide)) >= 0.5 * gravity * g_globals.frametime)
        return MRES_Ignored;

    // Slide velocity after collision
    float vel_slide[3];
    float incident_slide = GetVectorDotProduct(vel_before, normal_slide);
    OffsetVector(vel_before, normal_slide, -incident_slide, vel_slide);

    // Slide collision position
    float pos_slide[3];
    pos_slide = LoadFromVectorAddress(trace_slide.endpos);

    // Trace end at the bottom of the slide
    float pos_bottom[3];
    pos_bottom = LoadFromVectorAddress(trace_bottom.endpos);

    // Moved distance
    float pos_delta[3];
    SubtractVectors(pos_bottom, pos_slide, pos_delta);

    // How long it would take to reach the bottom by sliding down from the slide collision position
    float time = pos_delta[2] / vel_slide[2];

    // Subtrace time it would have taken to reach the bottom when starting right above the grounding height
    time -= GROUND_LAND_INTERVAL / vel_slide[2];

    // Furthest point back of the bottom
    float pos_intersection[3];
    OffsetVector(pos_slide, vel_slide, time, pos_intersection);

    // Current velocity
    float vel_move[3];
    vel_move = LoadFromVectorAddress(movedata_address + view_as<Address>(68));

    // Edgebug velocity
    float vel_edgebug[3];
    CopyVector(vel_move, vel_edgebug);
    vel_edgebug[2] = 0.0;

    // End of a full edgebug
    float pos_edgebug[3];
    OffsetVector(pos_intersection, vel_edgebug, g_globals.frametime, pos_edgebug);

    // Might want to support player scaling, idk
    float mins[3] = {-24.0, -24.0, 0.0};
    float maxs[3] = {24.0, 24.0, 62.0};

    float down[3];
    CopyVector(pos_edgebug, down);
    down[2] -= 2.0;

    TR_TraceHullFilter(pos_edgebug, down, mins, maxs, MASK_PLAYERSOLID_BRUSHONLY, TraceEntityFilterPlayer);

    // If there will be nothing to stand on after edgebugging, then let the player edgebug
    if (!TR_DidHit())
        return MRES_Supercede;

    return MRES_Ignored;
}





/*
_flushslopefix
███████ ██      ██    ██ ███████ ██   ██ ███████ ██       ██████  ██████  ███████ ███████ ██ ██   ██
██      ██      ██    ██ ██      ██   ██ ██      ██      ██    ██ ██   ██ ██      ██      ██  ██ ██ 
█████   ██      ██    ██ ███████ ███████ ███████ ██      ██    ██ ██████  █████   █████   ██   ███  
██      ██      ██    ██      ██ ██   ██      ██ ██      ██    ██ ██      ██      ██      ██  ██ ██ 
██      ███████  ██████  ███████ ██   ██ ███████ ███████  ██████  ██      ███████ ██      ██ ██   ██
*/





methodmap BrushSide
{
    public BrushSide(Address address)
    {
        return view_as<BrushSide>(address);
    }

    property Plane plane
    {
        public get()
        {
            return view_as<Plane>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(0), NumberType_Int32) );
        }
    }
}

methodmap BrushSideArray
{
    public BrushSideArray(Address address)
    {
        return view_as<BrushSideArray>(address);
    }

    public BrushSideArray Offset(int offset)
    {
        return view_as<BrushSideArray>(view_as<Address>(this) + view_as<Address>(8*offset));
    }

    public BrushSide Get(int index)
    {
        return view_as<BrushSide>(view_as<Address>(this) + view_as<Address>(8*index));
    }
}

methodmap Brush
{
    public Brush(Address address)
    {
        return view_as<Brush>(address);
    }

    property int numsides
    {
        public get()
        {
            return view_as<int>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(4), NumberType_Int16) );
        }
    }

    property int firstside
    {
        public get()
        {
            return view_as<int>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(6), NumberType_Int16) );
        }
    }
}

methodmap BSPData
{
    property BrushSideArray brushsides
    {
        public get()
        {
            return view_as<BrushSideArray>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(104), NumberType_Int32) );
        }
    }
}

methodmap TraceInfo
{
    public TraceInfo(Address address)
    {
        return view_as<TraceInfo>(address);
    }

    property Vector start
    {
        public get()
        {
            return view_as<Vector>( view_as<Address>(this) + view_as<Address>(0) );
        }
    }

    property Vector end
    {
        public get()
        {
            return view_as<Vector>( view_as<Address>(this) + view_as<Address>(12) );
        }
    }

    property Vector extents
    {
        public get()
        {
            return view_as<Vector>( view_as<Address>(this) + view_as<Address>(48) );
        }
    }

    property BSPData bspdata
    {
        public get()
        {
            return view_as<BSPData>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(260), NumberType_Int32) );
        }
    }
}





bool Flushslopefix_Start()
{
    if (!g_detours[DETOUR_CM_CLIPBOXTOBRUSH].Enable(Detour_Pre_CM_ClipBoxToBrush, Detour_Post_CM_ClipBoxToBrush))
        return false;

    return true;
}

void Flushslopefix_Stop()
{
    g_detours[DETOUR_CM_CLIPBOXTOBRUSH].Disable(Detour_Pre_CM_ClipBoxToBrush, Detour_Post_CM_ClipBoxToBrush);
}

Plane g_plane = view_as<Plane>(Address_Null);
float g_dist = 0.0;
MRESReturn Detour_Pre_CM_ClipBoxToBrush(DHookParam hParams)
{
    g_plane = view_as<Plane>(Address_Null);

    int client = g_player_simulating;
    if (!IsActivePlayer(client) || !g_inside_cmd)
        return MRES_Ignored;

    if (g_sessions[client].flushslopefix == 0)
        return MRES_Ignored;

    TraceInfo traceinfo = TraceInfo(DHookGetParamAddress(hParams, 1));
    Brush brush = Brush(DHookGetParamAddress(hParams, 2));
    int numsides = brush.numsides;

    if (numsides == 0xFFFF)
        return MRES_Ignored;

    float extents[3];
    extents = LoadFromVectorAddress(traceinfo.extents);

    float p1[3];
    p1 = LoadFromVectorAddress(traceinfo.start);

    float p2[3];
    p2 = LoadFromVectorAddress(traceinfo.end);

    int maxsides[2] = {-1, -1};
    float maxfracs[2] = {-Inf, -Inf};

    for (int side = 0; side < numsides; side++) {
        Plane plane = traceinfo.bspdata.brushsides.Offset(brush.firstside).Get(side).plane;

        float normal[3];
        normal = LoadFromVectorAddress(plane.normal);

        float dist = plane.dist + FloatAbs(normal[0]*extents[0]) + FloatAbs(normal[1]*extents[1]) + FloatAbs(normal[2]*extents[2]);

        float d1 = GetVectorDotProduct(normal, p1) - dist;
        float d2 = GetVectorDotProduct(normal, p2) - dist;

        if (d1 > 0.0 && d2 > 0.0)
            return MRES_Ignored;

        if (d1 > 0.0) {
            float fracs[2];
            fracs[0] = (d1) / (d1 - d2);
            fracs[1] = FloatMax(0.0, (d1 - DIST_EPSILON) / (d1 - d2));

            if (fracs[0] > maxfracs[0]) {
                maxsides[0] = side;
                maxfracs[0] = fracs[0];
            }

            if (fracs[1] > maxfracs[1]) {
                maxsides[1] = side;
                maxfracs[1] = fracs[1];
            }
        }
    }

    if (maxsides[0] == maxsides[1])
        return MRES_Ignored;

    Plane plane = traceinfo.bspdata.brushsides.Offset(brush.firstside).Get(maxsides[1]).plane;

    // Only axis-aligned planes are considered
    if (plane.type >= 3)
        return MRES_Ignored;

    // Don't care about pixelwalking
    if (g_sessions[g_player_simulating].flushslopefix == 1 && plane.normal.z == 1.0)
        return MRES_Ignored;

    g_plane = plane;
    g_dist = plane.dist;

    // Push out plane to prevent it from being used in collisions
    g_plane.dist += 5000.0;

    return MRES_Handled;
}

MRESReturn Detour_Post_CM_ClipBoxToBrush(DHookParam hParams)
{
    if (g_plane == view_as<Plane>(Address_Null))
        return MRES_Ignored;

    g_plane.dist = g_dist;

    g_plane = view_as<Plane>(Address_Null);

    return MRES_Handled;
}





/*
_sync
███████ ██    ██ ███    ██  ██████
██       ██  ██  ████   ██ ██     
███████   ████   ██ ██  ██ ██     
     ██    ██    ██  ██ ██ ██     
███████    ██    ██   ████  ██████
*/





Handle g_Call_Physics_SimulateEntity;
IPredictionSystem g_sync_te;
Address g_sync_movehelper;
bool Sync_Init()
{
    if (!g_sendvaredit_loaded)
        return SetError("Extension SendVarEdit is not present.");

    StartPrepSDKCall(SDKCall_Static);
    if (!PrepSDKCall_SetFromConf(g_gameconf, SDKConf_Signature, "Physics_SimulateEntity"))
        return SetError("Failed to prepare Physics_SimulateEntity call.");

    PrepSDKCall_AddParameter(SDKType_CBaseEntity, SDKPass_Pointer);

    g_Call_Physics_SimulateEntity = EndPrepSDKCall();
    if (g_Call_Physics_SimulateEntity == INVALID_HANDLE)
        return SetError("Failed to prepare Physics_SimulateEntity call.");

    g_sync_te = IPredictionSystem(GameConfGetAddress(g_gameconf, "te"));
    if (!g_sync_te)
        return SetError("Failed to find te.");

    g_sync_movehelper = GameConfGetAddress(g_gameconf, "&MoveHelperServer()::s_MoveHelperServer");
    if (!g_sync_movehelper)
        return SetError("Failed to find &MoveHelperServer()::s_MoveHelperServer.");

    return true;
}

bool Sync_Start()
{
    if (!g_detours[DETOUR_PHYSICS_SIMULATEENTITY].Enable(Sync_Detour_Pre_Physics_SimulateEntity, _))
        return false;

    if (!g_detours[DETOUR_UTIL_DECALTRACE].Enable(Sync_Detour_Pre_UTIL_DecalTrace, Sync_Detour_Post_UTIL_DecalTrace))
        return false;

    return true;
}

void Sync_Stop()
{
    g_detours[DETOUR_PHYSICS_SIMULATEENTITY].Disable(Sync_Detour_Pre_Physics_SimulateEntity, _);

    g_detours[DETOUR_UTIL_DECALTRACE].Disable(Sync_Detour_Pre_UTIL_DecalTrace, Sync_Detour_Post_UTIL_DecalTrace);
}

void Sync_OnPlayerRunCmdPost(int client)
{
    if (!IsActivePlayer(client))
        return;

    if (!g_sessions[client].sync)
        return;

    float curtime_player = g_globals.curtime;
    float frametime_player = g_globals.frametime;
    int tickcount_player = g_globals.tickcount;

    g_allow_update = true;

    for (int i = 0; i < g_numprojs[client]; i++) {
        int entity = g_projs[client][i].Entity();
        if (entity == -1)
            continue;

        if (!g_projs[client][i].IsPredictable())
            continue;

        g_globals.curtime = g_curtime_frame;
        g_globals.frametime = g_frametime_frame;
        g_globals.tickcount = g_tickcount_frame;

        int m_nSimulationTick_offset = FindDataMapInfo(0, "m_spawnflags") - 4; // m_nSimulationTick is right before m_spawnflags

        int m_nSimulationTick = GetEntData(entity, m_nSimulationTick_offset);
        SetEntData(entity, m_nSimulationTick_offset, g_tickcount_frame - 1);

        SDKCall(g_Call_Physics_SimulateEntity, entity);

        if (IsValidEdict(entity)) {
            // Don't simulate entity twice if sync turns off on the first frame
            if (m_nSimulationTick == -1)
                m_nSimulationTick = g_tickcount_frame - 1;

            SetEntData(entity, m_nSimulationTick_offset, m_nSimulationTick + 1);
        }
    }

    g_allow_update = false;

    // Set simulating entity back to the client
    g_entity_simulating = client;
    g_player_simulating = client;

    // If one of the player's projectiles has another player as their move parent, then m_pHostPlayer gets set to null after simulating the other player
    StoreToAddress(g_sync_movehelper + view_as<Address>(4), GetEntityAddress(client), NumberType_Int32, false);

    g_globals.curtime = curtime_player;
    g_globals.frametime = frametime_player;
    g_globals.tickcount = tickcount_player;
}

MRESReturn Sync_Detour_Pre_Physics_SimulateEntity(DHookParam hParams)
{
    int entity = DHookGetParam(hParams, 1);
    if (!IsValidEdict(entity))
        return MRES_Ignored;

    ProjectileInfo proj; proj = FindProjectile(entity);

    if (!IsActivePlayer(proj.client))
        return MRES_Ignored;

    if (!g_sessions[proj.client].sync)
        return MRES_Ignored;

    #if DEBUG
    if (g_debug_output != -1 && proj.client == g_debug_target && debug_toggles[DEBUG_SYNC]) {
        PrintToConsole(g_debug_output, "sync: %d - proj: %d, predictable: %s, allow update: %s", g_tickcount_frame, entity, g_projs[proj.client][proj.index].IsPredictable() ? "yes" : "no", g_allow_update ? "yes" : "no");
    }
    #endif

    if (!g_projs[proj.client][proj.index].IsPredictable())
        return MRES_Ignored;

    if (!g_allow_update)
        return MRES_Supercede;

    return MRES_Ignored;
}

// Implementation of SendProxy_SimulationTime
int SendProxySimulationTime(int entity)
{
    int addt = 0;
    int ticknumber = g_tickcount_frame;

    int mod = entity % 32;
    int tickbase = 100 * ((g_tickcount_frame - mod) / 100);

    if (ticknumber >= tickbase)
        addt = (ticknumber - tickbase) & 0xff;

    return addt;
}

void Sync_OnSendClientMessages()
{
    for (int client = 1; client <= MaxClients; client++) {
        if (!IsActivePlayer(client))
            continue;

        if (!g_sessions[client].sync)
            continue;

        for (int i = 0; i < g_numprojs[client]; i++) {
            int entity = g_projs[client][i].Entity();
            if (entity == -1)
                continue;

            if (!g_projs[client][i].IsPredictable())
                continue;

            // Marks entity as updated for the current frame for clients
            SetSendVar(entity, -1, "m_flSimulationTime", SendVarInt(SendProxySimulationTime(entity)), 1);

            ProjectileState state;
            state = g_projs[client][i].GetState(g_tickcount_frame);

            ProjectileState state_prev;
            state_prev = g_projs[client][i].GetState(g_tickcount_frame - 1);

            if (HasNetworkableProp(entity, "m_vecOrigin"))
                if (!CompareVectors(state.pos, state_prev.pos))
                    SetSendVar(entity, -1, "m_vecOrigin", SendVarVector(state.pos), 1);

            if (HasNetworkableProp(entity, "m_angRotation"))
                if (!CompareVectors(state.rot, state_prev.rot))
                    SetSendVar(entity, -1, "m_angRotation", SendVarVector(SendProxyQAngles(state.rot)), 1);

            if (HasNetworkableProp(entity, "m_vecVelocity"))
                if (!CompareVectors(state.vel, state_prev.vel))
                    SetSendVar(entity, -1, "m_vecVelocity", SendVarVector(state.vel), 1);

            if (HasNetworkableProp(entity, "m_vecAngVelocity"))
                if (!CompareVectors(state.angvel, state_prev.angvel))
                    SetSendVar(entity, -1, "m_vecAngVelocity", SendVarVector(SendProxyQAngles(state.angvel)), 1);
        }
    }
}

Address g_sync_suppresshost = Address_Null;
MRESReturn Sync_Detour_Pre_UTIL_DecalTrace(DHookParam hParams)
{
    g_sync_suppresshost = g_sync_te.m_pSuppressHost;

    if (g_entity_simulating == -1)
        return MRES_Ignored;

    ProjectileInfo proj; proj = FindProjectile(g_entity_simulating);
    if (!IsActivePlayer(proj.client))
        return MRES_Ignored;

    if (!g_sessions[proj.client].sync)
        return MRES_Ignored;

    g_sync_te.m_pSuppressHost = Address_Null;

    return MRES_Ignored;
}

MRESReturn Sync_Detour_Post_UTIL_DecalTrace(DHookParam hParams)
{
    g_sync_te.m_pSuppressHost = g_sync_suppresshost;

    return MRES_Ignored;
}





/*
_fakedelay
███████  █████  ██   ██ ███████ ██████  ███████ ██       █████  ██    ██
██      ██   ██ ██  ██  ██      ██   ██ ██      ██      ██   ██  ██  ██ 
█████   ███████ █████   █████   ██   ██ █████   ██      ███████   ████  
██      ██   ██ ██  ██  ██      ██   ██ ██      ██      ██   ██    ██   
██      ██   ██ ██   ██ ███████ ██████  ███████ ███████ ██   ██    ██   
*/





bool Fakedelay_Init()
{
    if (!g_sendvaredit_loaded)
        return SetError("Extension SendVarEdit is not present.");

    return true;
}

void Fakedelay_OnSendClientMessages()
{
    for (int client = 1; client <= MaxClients; client++) {
        if (!IsActivePlayer(client))
            continue;

        if (g_sessions[client].fakedelay < 0)
            continue;

        for (int i = 0; i < g_numprojs[client]; i++) {
            int entity = g_projs[client][i].Entity();
            if (entity == -1)
                continue;

            if (!g_projs[client][i].IsPredictable())
                continue;

            int frame = g_tickcount_frame + g_projs[client][i].clientdelay - RoundToCeil(g_sessions[client].fakedelay / 1000.0 / g_globals.interval_per_tick);

            ProjectileState state;
            state = g_projs[client][i].GetState(frame);

            ProjectileState state_prev;
            state_prev = g_projs[client][i].GetState(frame - 1);

            if (HasNetworkableProp(entity, "m_vecOrigin"))
                if (!CompareVectors(state.pos, state_prev.pos))
                    SetSendVar(entity, client, "m_vecOrigin", SendVarVector(state.pos), 3);

            if (HasNetworkableProp(entity, "m_angRotation"))
                if (!CompareVectors(state.rot, state_prev.rot))
                    SetSendVar(entity, client, "m_angRotation", SendVarVector(SendProxyQAngles(state.rot)), 3);

            if (HasNetworkableProp(entity, "m_vecVelocity"))
                if (!CompareVectors(state.vel, state_prev.vel))
                    SetSendVar(entity, client, "m_vecVelocity", SendVarVector(state.vel), 3);

            if (HasNetworkableProp(entity, "m_vecAngVelocity"))
                if (!CompareVectors(state.angvel, state_prev.angvel))
                    SetSendVar(entity, client, "m_vecAngVelocity", SendVarVector(SendProxyQAngles(state.angvel)), 3);
        }
    }
}