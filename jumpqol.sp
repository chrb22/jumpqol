#pragma semicolon 1
#pragma newdecls required

#include <sourcemod>
#include <dhooks>

public Plugin myinfo =
{
    name = "JumpQoL",
    author = "ILDPRUT",
    description = "Adds various improvements to jumping.",
    version = "1.0.4",
}





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





int g_offset_m_pServerClass;
SendProp GetSendProp(int entity, int index)
{
    Address m_Network = GetEntityAddress(entity) + view_as<Address>(20);
    Address m_pServerClass = view_as<Address>(LoadFromAddress(m_Network + view_as<Address>(g_offset_m_pServerClass), NumberType_Int32));
    Address m_pTable = view_as<Address>(LoadFromAddress(m_pServerClass + view_as<Address>(4), NumberType_Int32));
    Address m_pPrecalc = view_as<Address>(LoadFromAddress(m_pTable + view_as<Address>(12), NumberType_Int32));
    Address props = view_as<Address>(LoadFromAddress(m_pPrecalc + view_as<Address>(44), NumberType_Int32));
    return SendProp(LoadFromAddress(props + view_as<Address>(4*index), NumberType_Int32));
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
    DETOUR_FIREPIPEBOMB,
    DETOUR_UTIL_DECALTRACE,
    DETOUR_ITEMBUSYFRAME,
    DETOUR_SETGROUNDENTITY,
    DETOUR_CM_CLIPBOXTOBRUSH,
    DETOUR_SENDCLIENTMESSAGES,
    DETOUR_SV_COMPUTECLIENTPACKS,
    DETOUR_SENDSNAPSHOT,
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
            if (active && this.numusing == 0) {
                bool success;
                Call_StartFunction(INVALID_HANDLE, this.f_start);
                Call_Finish(success);

                if (!success) {
                    Call_StartFunction(INVALID_HANDLE, this.f_stop);
                    Call_Finish();

                    return false;
                }
            }
            else if (!active && this.numusing == 1) {
                Call_StartFunction(INVALID_HANDLE, this.f_stop);
                Call_Finish();
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
    SETTING_RANDPROJVEL,
    SETTING_RANDPROJANGVEL,
    SETTING_PROJDECALS,
    SETTING_SHOWDETDECALS,
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





enum struct SendPropInfo
{
    SendProp prop;

    int bit;
}

enum struct ProjectileMessage
{
    // No enum struct arrays inside enum structs :(
    SendPropInfo pos;
    SendPropInfo rot;
    SendPropInfo vel;
    SendPropInfo angvel;
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

    ProjectileMessage message;

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

        int total = 0;
        total += view_as<int>(HasEntProp(entity, Prop_Send, "m_vecOrigin"));
        total += view_as<int>(HasEntProp(entity, Prop_Send, "m_angRotation"));
        total += view_as<int>(HasEntProp(entity, Prop_Send, "m_vecVelocity"));
        total += view_as<int>(HasEntProp(entity, Prop_Send, "m_vecAngVelocity"));

        SendPropInfo info;
        info.prop = SendProp(Address_Null);
        info.bit = -1;

        this.message.pos = info;
        this.message.rot = info;
        this.message.vel = info;
        this.message.vel = info;

        int i = 0;
        int count = 0;
        while (count < total) {
            SendProp prop = GetSendProp(entity, i);
            
            bool match = true;
            if (prop.offset == FindDataMapInfo(0, "m_vecOrigin"))
                this.message.pos.prop = prop;
            else if (prop.offset == FindDataMapInfo(0, "m_angRotation"))
                this.message.rot.prop = prop;
            else if (prop.offset == FindDataMapInfo(0, "m_vecVelocity"))
                this.message.vel.prop = prop;
            else if (prop.offset == FindDataMapInfo(0, "m_vecAngVelocity"))
                this.message.angvel.prop = prop;
            else
                match = false;

            if (match)
                count++;

            i++;
        }
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

    property bool randprojvel
    {
        public get()
        {
            return g_settings[SETTING_RANDPROJVEL].values[this.client];
        }
    }

    property bool randprojangvel
    {
        public get()
        {
            return g_settings[SETTING_RANDPROJANGVEL].values[this.client];
        }
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
    g_detours[DETOUR_FIREPIPEBOMB].Init(
        "CTFWeaponBaseGun::FirePipeBomb",
        CallConv_THISCALL, ReturnType_CBaseEntity, ThisPointer_CBaseEntity,
        {HookParamType_CBaseEntity, HookParamType_Int, HookParamType_Unknown}
    );
    g_detours[DETOUR_UTIL_DECALTRACE].Init(
        "UTIL_DecalTrace",
        CallConv_CDECL, ReturnType_Void, ThisPointer_Ignore,
        {HookParamType_Int, HookParamType_CharPtr, HookParamType_Unknown}
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
    g_detours[DETOUR_SENDCLIENTMESSAGES].Init(
        "CGameServer::SendClientMessages",
        CallConv_THISCALL, ReturnType_Void, ThisPointer_Address,
        {HookParamType_Bool, HookParamType_Unknown}
    );
    g_detours[DETOUR_SV_COMPUTECLIENTPACKS].Init(
        "SV_ComputeClientPacks",
        CallConv_CDECL, ReturnType_Void, ThisPointer_Ignore,
        {HookParamType_Int, HookParamType_Int, HookParamType_ObjectPtr, HookParamType_Unknown}
    );
    g_detours[DETOUR_SENDSNAPSHOT].Init(
        "CGameClient::SendSnapshot",
        CallConv_THISCALL, ReturnType_Void, ThisPointer_Address,
        {HookParamType_ObjectPtr, HookParamType_Unknown}
    );

    // Required
    if (!Required_Init())
        SetFailState("[jumpqol.smx] Unable to start plugin: %s", g_error);

    // Settings
    RegConsoleCmd("sm_jumpqol", Command_Plugin);

    g_settings[SETTING_RANDPROJVEL].name = "randprojvel";
    g_settings[SETTING_RANDPROJVEL].desc = "Controls if pipes and stickies should have some spread when fired.";
    g_settings[SETTING_RANDPROJVEL].expl = "";
    g_settings[SETTING_RANDPROJVEL].type = SETTING_BOOL;
    g_settings[SETTING_RANDPROJVEL].f_init = Randprojvel_Init;
    g_settings[SETTING_RANDPROJVEL].f_start = Randprojvel_Start;
    g_settings[SETTING_RANDPROJVEL].f_stop = Randprojvel_Stop;
    g_settings[SETTING_RANDPROJVEL].f_active = SettingActiveDefaultBoolNeg;
    g_settings[SETTING_RANDPROJVEL].range[0] = 0.0;
    g_settings[SETTING_RANDPROJVEL].range[1] = 1.0;
    g_settings[SETTING_RANDPROJVEL].Init(true, true);

    g_settings[SETTING_RANDPROJANGVEL].name = "randprojangvel";
    g_settings[SETTING_RANDPROJANGVEL].desc = "Controls if pipes and stickies should be fired with a random angular velocity.";
    g_settings[SETTING_RANDPROJANGVEL].expl = "";
    g_settings[SETTING_RANDPROJANGVEL].type = SETTING_BOOL;
    g_settings[SETTING_RANDPROJANGVEL].f_init = Randprojangvel_Init;
    g_settings[SETTING_RANDPROJANGVEL].f_start = Randprojangvel_Start;
    g_settings[SETTING_RANDPROJANGVEL].f_stop = Randprojangvel_Stop;
    g_settings[SETTING_RANDPROJANGVEL].f_active = SettingActiveDefaultBoolNeg;
    g_settings[SETTING_RANDPROJANGVEL].range[0] = 0.0;
    g_settings[SETTING_RANDPROJANGVEL].range[1] = 1.0;
    g_settings[SETTING_RANDPROJANGVEL].Init(true, true);

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
    g_settings[SETTING_FAKEDELAY].f_start = Fakedelay_Start;
    g_settings[SETTING_FAKEDELAY].f_stop = Fakedelay_Stop;
    g_settings[SETTING_FAKEDELAY].f_active = SettingActiveDefaultIntGE;
    g_settings[SETTING_FAKEDELAY].range[0] = -1.0;
    g_settings[SETTING_FAKEDELAY].range[1] = NUM_PREDICTIONS*g_globals.interval_per_tick*1000.0;
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
}





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





Handle g_Required_Call_CBaseFilter__PassesFilter;
bool Required_Init()
{
    g_globals = Globals(GameConfGetAddress(g_gameconf, "gpGlobals"));
    if (!g_globals)
        return SetError("Failed to find gpGlobals.");

    g_offset_m_pServerClass = GameConfGetOffset(g_gameconf, "CServerNetworkProperty::m_pServerClass");
    if (g_offset_m_pServerClass == -1)
        return SetError("Failed to get CServerNetworkProperty::m_pServerClass offset.");

    if (!g_detours[DETOUR_PHYSICS_SIMULATEENTITY].Enable(Required_Detour_Pre_Physics_SimulateEntity, Required_Detour_Post_Physics_SimulateEntity))
        return false;

    if (!g_detours[DETOUR_SERVICEEVENTS].Enable(Required_Detour_Pre_CEventQueue__ServiceEvents, _))
        return false;

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
_randprojvel
██████   █████  ███    ██ ██████  ██████  ██████   ██████       ██ ██    ██ ███████ ██      
██   ██ ██   ██ ████   ██ ██   ██ ██   ██ ██   ██ ██    ██      ██ ██    ██ ██      ██      
██████  ███████ ██ ██  ██ ██   ██ ██████  ██████  ██    ██      ██ ██    ██ █████   ██      
██   ██ ██   ██ ██  ██ ██ ██   ██ ██      ██   ██ ██    ██ ██   ██  ██  ██  ██      ██      
██   ██ ██   ██ ██   ████ ██████  ██      ██   ██  ██████   █████    ████   ███████ ███████
*/





Address g_pipebomb_rand_right_address[2];
float g_pipebomb_rand_right_value[2];
Address g_pipebomb_rand_up_address[2];
float g_pipebomb_rand_up_value[2];
bool Randprojvel_Init()
{
    g_pipebomb_rand_right_address[0] = GameConfGetAddress(g_gameconf, "CTFWeaponBaseGun::FirePipeBomb rand right min");
    g_pipebomb_rand_right_address[1] = GameConfGetAddress(g_gameconf, "CTFWeaponBaseGun::FirePipeBomb rand right max");
    if (!g_pipebomb_rand_right_address[0] || !g_pipebomb_rand_right_address[1])
        return SetError("Failed to find CTFWeaponBaseGun::FirePipeBomb rand right.");

    g_pipebomb_rand_right_value[0] = LoadFromAddress(g_pipebomb_rand_right_address[0], NumberType_Int32);
    g_pipebomb_rand_right_value[1] = LoadFromAddress(g_pipebomb_rand_right_address[1], NumberType_Int32);

    g_pipebomb_rand_up_address[0] = GameConfGetAddress(g_gameconf, "CTFWeaponBaseGun::FirePipeBomb rand up min");
    g_pipebomb_rand_up_address[1] = GameConfGetAddress(g_gameconf, "CTFWeaponBaseGun::FirePipeBomb rand up max");
    if (!g_pipebomb_rand_up_address[0] || !g_pipebomb_rand_up_address[1])
        return SetError("Failed to find CTFWeaponBaseGun::FirePipeBomb rand up.");

    g_pipebomb_rand_up_value[0] = LoadFromAddress(g_pipebomb_rand_up_address[0], NumberType_Int32);
    g_pipebomb_rand_up_value[1] = LoadFromAddress(g_pipebomb_rand_up_address[1], NumberType_Int32);

    return true;
}

bool Randprojvel_Start()
{
    if (!g_detours[DETOUR_FIREPIPEBOMB].Enable(Randprojvel_Detour_Pre_CTFWeaponBaseGun__FirePipeBomb, Randprojvel_Detour_Post_CTFWeaponBaseGun__FirePipeBomb))
        return false;

    return true;
}

void Randprojvel_Stop()
{
    g_detours[DETOUR_FIREPIPEBOMB].Disable(Randprojvel_Detour_Pre_CTFWeaponBaseGun__FirePipeBomb, Randprojvel_Detour_Post_CTFWeaponBaseGun__FirePipeBomb);
}

MRESReturn Randprojvel_Detour_Pre_CTFWeaponBaseGun__FirePipeBomb(int entity, DHookReturn hReturn, DHookParam hParams)
{
    int client = GetEntPropEnt(entity, Prop_Data, "m_hOwner");
    if (!IsActivePlayer(client))
        return MRES_Ignored;

    if (g_sessions[client].randprojvel)
        return MRES_Ignored;        

    StoreToAddress(g_pipebomb_rand_right_address[0], 0.0, NumberType_Int32);
    StoreToAddress(g_pipebomb_rand_right_address[1], 0.0, NumberType_Int32);

    StoreToAddress(g_pipebomb_rand_up_address[0], 0.0, NumberType_Int32);
    StoreToAddress(g_pipebomb_rand_up_address[1], 0.0, NumberType_Int32);

    return MRES_Ignored;
}

MRESReturn Randprojvel_Detour_Post_CTFWeaponBaseGun__FirePipeBomb(int entity, DHookReturn hReturn, DHookParam hParams)
{
    int client = GetEntPropEnt(entity, Prop_Data, "m_hOwner");
    if (!IsActivePlayer(client))
        return MRES_Ignored;

    if (!g_sessions[client].randprojvel)
        return MRES_Ignored;

    StoreToAddress(g_pipebomb_rand_right_address[0], g_pipebomb_rand_right_value[0], NumberType_Int32);
    StoreToAddress(g_pipebomb_rand_right_address[1], g_pipebomb_rand_right_value[1], NumberType_Int32);

    StoreToAddress(g_pipebomb_rand_up_address[0], g_pipebomb_rand_up_value[0], NumberType_Int32);
    StoreToAddress(g_pipebomb_rand_up_address[1], g_pipebomb_rand_up_value[1], NumberType_Int32);

    return MRES_Handled;
}





/*
_randprojangvel
██████   █████  ███    ██ ██████  ██████  ██████   ██████       ██  █████  ███    ██  ██████  ██    ██ ███████ ██      
██   ██ ██   ██ ████   ██ ██   ██ ██   ██ ██   ██ ██    ██      ██ ██   ██ ████   ██ ██       ██    ██ ██      ██      
██████  ███████ ██ ██  ██ ██   ██ ██████  ██████  ██    ██      ██ ███████ ██ ██  ██ ██   ███ ██    ██ █████   ██      
██   ██ ██   ██ ██  ██ ██ ██   ██ ██      ██   ██ ██    ██ ██   ██ ██   ██ ██  ██ ██ ██    ██  ██  ██  ██      ██      
██   ██ ██   ██ ██   ████ ██████  ██      ██   ██  ██████   █████  ██   ██ ██   ████  ██████    ████   ███████ ███████
*/





Address g_pipebomb_rand_yaw_address[2];
int g_pipebomb_rand_yaw_value[2];
bool Randprojangvel_Init()
{
    g_pipebomb_rand_yaw_address[0] = GameConfGetAddress(g_gameconf, "CTFWeaponBaseGun::FirePipeBomb rand yaw min");
    g_pipebomb_rand_yaw_address[1] = GameConfGetAddress(g_gameconf, "CTFWeaponBaseGun::FirePipeBomb rand yaw max");
    if (!g_pipebomb_rand_yaw_address[0] || !g_pipebomb_rand_yaw_address[1])
        return SetError("Failed to find CTFWeaponBaseGun::FirePipeBomb rand yaw.");

    g_pipebomb_rand_yaw_value[0] = LoadFromAddress(g_pipebomb_rand_yaw_address[0], NumberType_Int32);
    g_pipebomb_rand_yaw_value[1] = LoadFromAddress(g_pipebomb_rand_yaw_address[1], NumberType_Int32);

    return true;
}

bool Randprojangvel_Start()
{
    if (!g_detours[DETOUR_FIREPIPEBOMB].Enable(Randprojangvel_Detour_Pre_CTFWeaponBaseGun__FirePipeBomb, Randprojangvel_Detour_Post_CTFWeaponBaseGun__FirePipeBomb))
        return false;

    return true;
}

void Randprojangvel_Stop()
{
    g_detours[DETOUR_FIREPIPEBOMB].Disable(Randprojangvel_Detour_Pre_CTFWeaponBaseGun__FirePipeBomb, Randprojangvel_Detour_Post_CTFWeaponBaseGun__FirePipeBomb);
}

MRESReturn Randprojangvel_Detour_Pre_CTFWeaponBaseGun__FirePipeBomb(int entity, DHookReturn hReturn, DHookParam hParams)
{
    int client = GetEntPropEnt(entity, Prop_Data, "m_hOwner");
    if (!IsActivePlayer(client))
        return MRES_Ignored;

    if (g_sessions[client].randprojvel)
        return MRES_Ignored; 

    StoreToAddress(g_pipebomb_rand_yaw_address[0], 0, NumberType_Int32);
    StoreToAddress(g_pipebomb_rand_yaw_address[1], 0, NumberType_Int32);

    return MRES_Ignored;
}

MRESReturn Randprojangvel_Detour_Post_CTFWeaponBaseGun__FirePipeBomb(int entity, DHookReturn hReturn, DHookParam hParams)
{
    int client = GetEntPropEnt(entity, Prop_Data, "m_hOwner");
    if (!IsActivePlayer(client))
        return MRES_Ignored;

    if (!g_sessions[client].randprojvel)
        return MRES_Ignored;

    StoreToAddress(g_pipebomb_rand_yaw_address[0], g_pipebomb_rand_yaw_value[0], NumberType_Int32);
    StoreToAddress(g_pipebomb_rand_yaw_address[1], g_pipebomb_rand_yaw_value[1], NumberType_Int32);

    return MRES_Handled;
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
_reloadfire
██████  ███████ ██       ██████   █████  ██████  ███████ ██ ██████  ███████ 
██   ██ ██      ██      ██    ██ ██   ██ ██   ██ ██      ██ ██   ██ ██      
██████  █████   ██      ██    ██ ███████ ██   ██ █████   ██ ██████  █████   
██   ██ ██      ██      ██    ██ ██   ██ ██   ██ ██      ██ ██   ██ ██      
██   ██ ███████ ███████  ██████  ██   ██ ██████  ██      ██ ██   ██ ███████ 
*/





Handle g_Call_ItemPostFrame;
bool Reloadfire_Init()
{
    StartPrepSDKCall(SDKCall_Entity);
    if (!PrepSDKCall_SetFromConf(g_gameconf, SDKConf_Virtual, "CBaseCombatWeapon::ItemPostFrame"))
        return SetError("Failed to prepare CBaseCombatWeapon::ItemPostFrame call.");

    g_Call_ItemPostFrame = EndPrepSDKCall();
    if (g_Call_ItemPostFrame == INVALID_HANDLE)
        return SetError("Failed to prepare CBaseCombatWeapon::ItemPostFrame call.");

    return true;
}

bool Reloadfire_Start()
{
    if (!g_detours[DETOUR_ITEMBUSYFRAME].Enable(_, Reloadfire_Detour_Post_CTFWeaponBase__ItemBusyFrame))
        return false;

    return true;
}

void Reloadfire_Stop()
{
    g_detours[DETOUR_ITEMBUSYFRAME].Disable(_, Reloadfire_Detour_Post_CTFWeaponBase__ItemBusyFrame);
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


    int m_TouchList_m_Size = LoadFromAddress(g_slideebfix_movehelper + view_as<Address>(8) + view_as<Address>(16), NumberType_Int32);
    if (m_TouchList_m_Size == 0)
        return MRES_Ignored; // No collisions during this tick

    Address m_TouchList_m_pElements = LoadFromAddress(g_slideebfix_movehelper + view_as<Address>(8) + view_as<Address>(20), NumberType_Int32);

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

    if (!g_detours[DETOUR_SENDCLIENTMESSAGES].Enable(Sync_Detour_Pre_CGameServer__SendClientMessages, Sync_Detour_Post_CGameServer__SendClientMessages))
        return false;

    if (!g_detours[DETOUR_UTIL_DECALTRACE].Enable(Sync_Detour_Pre_UTIL_DecalTrace, Sync_Detour_Post_UTIL_DecalTrace))
        return false;

    return true;
}

void Sync_Stop()
{
    g_detours[DETOUR_PHYSICS_SIMULATEENTITY].Disable(Sync_Detour_Pre_Physics_SimulateEntity, _);

    g_detours[DETOUR_SENDCLIENTMESSAGES].Disable(Sync_Detour_Pre_CGameServer__SendClientMessages, Sync_Detour_Post_CGameServer__SendClientMessages);

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

        if (IsValidEdict(entity))
            SetEntData(entity, m_nSimulationTick_offset, m_nSimulationTick + 1);
    }

    g_allow_update = false;

    // Set simulating entity back to the client
    g_entity_simulating = client;
    g_player_simulating = client;

    // If one of the player's projectiles has another player as their move parent, then m_pHostPlayer gets set to null after simulating the other player
    StoreToAddress(g_sync_movehelper + view_as<Address>(8), GetEntityAddress(client), NumberType_Int32, false);

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

    if (!g_projs[proj.client][proj.index].IsPredictable())
        return MRES_Ignored;

    if (!g_allow_update)
        return MRES_Supercede;

    return MRES_Ignored;
}

MRESReturn Sync_Detour_Pre_CGameServer__SendClientMessages(Address pThis, DHookParam hParams)
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

            SetEntPropFloat(entity, Prop_Data, "m_flSimulationTime", g_curtime_frame); // Marks entity as updated for the current frame
            g_projs[client][i].GetState(g_tickcount_frame).WriteTo(entity);
        }
    }

    return MRES_Ignored;
}

MRESReturn Sync_Detour_Post_CGameServer__SendClientMessages(Address pThis, DHookParam hParams)
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

            int frame = g_projs[client][i].frame;
            g_projs[client][i].GetState(frame).WriteTo(entity);
        }
    }

    return MRES_Ignored;
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





// From coordsize.h
#define COORD_INTEGER_BITS          14
#define COORD_FRACTIONAL_BITS       5
#define COORD_DENOMINATOR           (1<<(COORD_FRACTIONAL_BITS))
#define COORD_RESOLUTION            (1.0/(COORD_DENOMINATOR))

#define COORD_INTEGER_BITS_MP       11
#define COORD_FRACTIONAL_BITS_MP_LOWPRECISION 3
#define COORD_DENOMINATOR_LOWPRECISION          (1<<(COORD_FRACTIONAL_BITS_MP_LOWPRECISION))
#define COORD_RESOLUTION_LOWPRECISION           (1.0/(COORD_DENOMINATOR_LOWPRECISION))

#define NORMAL_FRACTIONAL_BITS      11
#define NORMAL_DENOMINATOR          ( (1<<(NORMAL_FRACTIONAL_BITS)) - 1 )
#define NORMAL_RESOLUTION           (1.0/(NORMAL_DENOMINATOR))

// From dt_common.h
#define SPROP_UNSIGNED                  (1<<0)
#define SPROP_COORD                     (1<<1)
#define SPROP_NOSCALE                   (1<<2)
#define SPROP_ROUNDDOWN                 (1<<3)
#define SPROP_ROUNDUP                   (1<<4)
#define SPROP_NORMAL                    (1<<5)
#define SPROP_EXCLUDE                   (1<<6)
#define SPROP_XYZE                      (1<<7)
#define SPROP_INSIDEARRAY               (1<<8)
#define SPROP_PROXY_ALWAYS_YES          (1<<9)
#define SPROP_CHANGES_OFTEN             (1<<10)
#define SPROP_IS_A_VECTOR_ELEM          (1<<11)
#define SPROP_COLLAPSIBLE               (1<<12)
#define SPROP_COORD_MP                  (1<<13)
#define SPROP_COORD_MP_LOWPRECISION     (1<<14)
#define SPROP_COORD_MP_INTEGRAL         (1<<15)

bool UseRealCoordMP(SendProp prop)
{
    return prop.flags & (SPROP_COORD_MP | SPROP_COORD_MP_LOWPRECISION | SPROP_COORD_MP_INTEGRAL) ? false : true;
}

methodmap BitBuffer
{
    public BitBuffer(Address address)
    {
        return view_as<BitBuffer>(address);
    }

    // From bf_read::ReadUBitLong
    public int Read(int bit, int numbits)
    {
        int iStartBit = bit & 31;
        int iLastBit = bit + numbits - 1;
        int iWordOffset1 = bit >>> 5;
        int iWordOffset2 = iLastBit >>> 5;
    
        int bitmask = (2 << (numbits - 1)) - 1;

        Address dw1_address = view_as<Address>(this) + view_as<Address>(iWordOffset1*4);
        Address dw2_address = view_as<Address>(this) + view_as<Address>(iWordOffset2*4);

        int dw1 = LoadFromAddress(dw1_address, NumberType_Int32) >>> iStartBit;
        int dw2 = LoadFromAddress(dw2_address, NumberType_Int32) << (32 - iStartBit);

        return (dw1 | dw2) & bitmask;
    }

    // From bf_write::WriteUBitLong
    public int Write(int bit, int value, int numbits)
    {
        int iCurBitMasked = bit & 31;
        int iDWord = bit >>> 5;

        value = (value << iCurBitMasked) | (value >>> (32 - iCurBitMasked));

        int temp = 1 << (numbits-1);
        int mask1 = (temp*2-1) << iCurBitMasked;
        int mask2 = (temp-1) >>> (31 - iCurBitMasked);
        
        int i = mask2 & 1;
        Address dword1_address = view_as<Address>(this) + view_as<Address>((iDWord + 0)*4);
        Address dword2_address = view_as<Address>(this) + view_as<Address>((iDWord + i)*4);

        int dword1 = LoadFromAddress( dword1_address, NumberType_Int32);
        int dword2 = LoadFromAddress( dword2_address, NumberType_Int32);
        
        dword1 ^= ( mask1 & ( value ^ dword1 ) );
        dword2 ^= ( mask2 & ( value ^ dword2 ) );

        StoreToAddress(dword2_address, dword2, NumberType_Int32, false);
        StoreToAddress(dword1_address, dword1, NumberType_Int32, false);

        return numbits;
    }

    // Mostly taken from bitbuf.cpp
    public int WriteFloat(int bit, float value, int flags, bool realcoordmp = false, int numbits = -1)
    {
        int start = bit;

        bool lowprecision = ((flags & SPROP_COORD_MP_LOWPRECISION) != 0);

        int signbit = (value <= -(lowprecision ? COORD_RESOLUTION_LOWPRECISION : COORD_RESOLUTION));
        int intval = RoundToFloor(FloatAbs(value));
        int fractval = lowprecision ?
                            RoundToFloor(FloatAbs(value * COORD_DENOMINATOR_LOWPRECISION)) & (COORD_DENOMINATOR_LOWPRECISION-1) :
                            RoundToFloor(FloatAbs(value * COORD_DENOMINATOR)) & (COORD_DENOMINATOR-1);
        int inbounds = (intval < (1 << COORD_INTEGER_BITS_MP));

        if (flags & SPROP_COORD) {
            bit += this.Write(bit, intval != 0, 1);
            bit += this.Write(bit, fractval != 0, 1);

            if (intval != 0 || fractval != 0) {
                bit += this.Write(bit, signbit != 0, 1);

                if (intval != 0) {
                    intval--;
                    bit += this.Write(bit, intval, COORD_INTEGER_BITS);
                }

                if (fractval != 0) {
                    bit += this.Write(bit, fractval, COORD_FRACTIONAL_BITS);
                }
            }
        }
        else if (flags & (SPROP_COORD_MP | SPROP_COORD_MP_LOWPRECISION | SPROP_COORD_MP_INTEGRAL)) {
            int bits = 0;
            numbits = 0;

            // We want to always use COORD_INTEGER_BITS bits (reserved by using 2048.0 in Fakedelay_Detour_Pre_SV_ComputeClientPacks)
            if (!realcoordmp) {
                inbounds = false;
                if (intval == 0)
                    intval = 1;
            }

            if (flags & SPROP_COORD_MP_INTEGRAL) {
                if (intval != 0) {
                    intval--;
                    bits = intval * 8 + signbit * 4 + 2 + inbounds;
                    numbits = 3 + (inbounds ? COORD_INTEGER_BITS_MP : COORD_INTEGER_BITS);
                }
                else {
                    bits = inbounds;
                    numbits = 2;
                }
            }
            else {
                if (intval != 0) {
                    intval--;
                    bits = intval * 8 + signbit * 4 + 2 + inbounds;
                    bits += inbounds ? (fractval << (3+COORD_INTEGER_BITS_MP)) : (fractval << (3+COORD_INTEGER_BITS));
                    numbits = 3 + (inbounds ? COORD_INTEGER_BITS_MP : COORD_INTEGER_BITS)
                                + (lowprecision ? COORD_FRACTIONAL_BITS_MP_LOWPRECISION : COORD_FRACTIONAL_BITS);
                }
                else {
                    bits = fractval * 8 + signbit * 4 + 0 + inbounds;
                    numbits = 3 + (lowprecision ? COORD_FRACTIONAL_BITS_MP_LOWPRECISION : COORD_FRACTIONAL_BITS);
                }
            }

            bit += this.Write(bit, bits, numbits);
        }
        else if (flags & SPROP_NORMAL) {
            signbit = (value <= -NORMAL_RESOLUTION);
            fractval = RoundToFloor(FloatAbs(value * NORMAL_DENOMINATOR));

            if (fractval > NORMAL_DENOMINATOR)
                fractval = NORMAL_DENOMINATOR;

            bit += this.Write(bit, signbit, 1);
            bit += this.Write(bit, fractval, NORMAL_FRACTIONAL_BITS);
        }
        else if (flags & SPROP_NOSCALE) {
            bit += this.Write(bit, view_as<int>(value), 32);
        }
        else {
            if (numbits == -1)
                SetFailState("Need to properly pass ranged floats.");

            bit += this.Write(bit, RoundFloat(value), numbits);
        }

        return bit - start;
    }

    public int WritePropFloat(SendPropInfo info, int bit, float value)
    {
        int numbits = -1;
        if (info.prop.flags & (SPROP_COORD | SPROP_COORD_MP | SPROP_COORD_MP_LOWPRECISION | SPROP_COORD_MP_INTEGRAL | SPROP_NORMAL | SPROP_NOSCALE) == 0) {
            value = (value - info.prop.lowvalue) * info.prop.highlowmul;
            numbits = info.prop.numbits;
        }

        return this.WriteFloat(bit, value, info.prop.flags, UseRealCoordMP(info.prop), numbits);
    }

    public int WritePropVector(SendPropInfo info, int bit, float vector[3])
    {
        int start = bit;

        bit += this.WritePropFloat(info, bit, vector[0]);
        bit += this.WritePropFloat(info, bit, vector[1]);
        if (info.prop.flags & SPROP_NORMAL == 0)
            bit += this.WritePropFloat(info, bit, vector[2]);
        else
            bit += this.Write(bit, view_as<int>(vector[2] <= -NORMAL_RESOLUTION), 1);

        return bit - start;
    }
}

methodmap PackedEntity
{
    property int entity
    {
        public get()
        {
            return view_as<int>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(8), NumberType_Int32) );
        }
    }

    property BitBuffer data
    {
        public get()
        {
            return view_as<BitBuffer>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(36), NumberType_Int32) );
        }
    }

    property int numbits
    {
        public get()
        {
            return view_as<int>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(40), NumberType_Int32) );
        }
    }
};

methodmap PackedEntityPointerArray
{
    public PackedEntityPointerArray(Address address)
    {
        return view_as<PackedEntityPointerArray>(address);
    }

    public PackedEntity Get(int index)
    {
        return view_as<PackedEntity>( LoadFromAddress(view_as<Address>(this) + view_as<Address>(4*index), NumberType_Int32) );
    }
}

methodmap CFrameSnapshotManager
{
    property PackedEntityPointerArray packeddata
    {
        public get()
        {
            return view_as<PackedEntityPointerArray>( view_as<Address>(this) + view_as<Address>(104) );
        }
    }
};
CFrameSnapshotManager g_framesnapshotmanager;





enum struct bf_read
{
    BitBuffer m_pData;
    int m_nDataBytes;
    int m_nDataBits;
    int m_iCurBit;
    int overflow;
    Address m_pDebugName;
}

// Looks through SendProps of a packed entity like SendTable_CalcDelta does
Handle g_Call_SkipProp[7];
bool FindProjectileBitOffsets(int client, int index)
{
    int entity = g_projs[client][index].Entity();

    PackedEntity packed = g_framesnapshotmanager.packeddata.Get(entity);
    if (packed == view_as<PackedEntity>(Address_Null))
        return false;

    BitBuffer buffer = packed.data;
    int numbits = packed.numbits;

    bf_read read;
    read.m_pData = buffer;
    read.m_nDataBytes = RoundToCeil(float(numbits) / 8.0);
    read.m_nDataBits = numbits;
    read.m_iCurBit = 0;
    read.overflow = 0;
    read.m_pDebugName = Address_Null;

    int total = 0;
    total += view_as<int>(!!g_projs[client][index].message.pos.prop);
    total += view_as<int>(!!g_projs[client][index].message.rot.prop);
    total += view_as<int>(!!g_projs[client][index].message.vel.prop);
    total += view_as<int>(!!g_projs[client][index].message.angvel.prop);

    int i = -1;
    SendProp prop;

    int count = 0;
    while (count < total && read.m_nDataBits - read.m_iCurBit >= 7) {
        int bit = read.m_pData.Read(read.m_iCurBit, 1);
        read.m_iCurBit += 1;

        if (bit == 0)
            break;

        // bf_read::ReadUBitVar
        int bits = read.m_pData.Read(read.m_iCurBit, 6);
        read.m_iCurBit += 6;

        int delta = bits >>> 2;
        if (bits & 3) {
            int numarr[4] = {4, 8, 12, 32};
            numbits = numarr[bits & 3];

            read.m_iCurBit -= 4;

            delta = read.m_pData.Read(read.m_iCurBit, numbits);
            read.m_iCurBit += numbits;
        }

        i += 1 + delta;

        prop = GetSendProp(entity, i);

        bool match = true;
        if (prop == g_projs[client][index].message.pos.prop)
            g_projs[client][index].message.pos.bit = read.m_iCurBit;
        else if (prop == g_projs[client][index].message.rot.prop)
            g_projs[client][index].message.rot.bit = read.m_iCurBit;
        else if (prop == g_projs[client][index].message.vel.prop)
            g_projs[client][index].message.vel.bit = read.m_iCurBit;
        else if (prop == g_projs[client][index].message.angvel.prop)
            g_projs[client][index].message.angvel.bit = read.m_iCurBit;
        else
            match = false;

        if (match)
            count++;

        SDKCall(g_Call_SkipProp[prop.type], prop, read);
    }

    return true;
}





bool Fakedelay_Init()
{   
    ConVar sv_parallel_sendsnapshot = FindConVar("sv_parallel_sendsnapshot");
    HookConVarChange(sv_parallel_sendsnapshot, Fakedelay_Hook_sv_parallel_sendsnapshot);

    if (sv_parallel_sendsnapshot.BoolValue == true)
        return SetError("ConVar sv_parallel_sendsnapshot must be set to 0.");

    g_framesnapshotmanager = view_as<CFrameSnapshotManager>(GameConfGetAddress(g_gameconf, "framesnapshotmanager"));
    if (!g_framesnapshotmanager)
        return SetError("Failed to find framesnapshotmanager.");

    Address proptypefns = GameConfGetAddress(g_gameconf, "&g_PropTypeFns");
    if (!proptypefns)
        return SetError("Failed to find &g_PropTypeFns.");

    // CALL SkipProp
    for (int type = 0; type < 7; type++) {
        Address address = LoadFromAddress(proptypefns + view_as<Address>(36*type) + view_as<Address>(32), NumberType_Int32);

        if (!address)
            continue;

        StartPrepSDKCall(SDKCall_Static);
        if (!PrepSDKCall_SetAddress(address))
            return SetError("Failed to prepare SkipProp call.");

        PrepSDKCall_AddParameter(SDKType_PlainOldData, SDKPass_Plain);
        PrepSDKCall_AddParameter(SDKType_String, SDKPass_Pointer);

        g_Call_SkipProp[type] = EndPrepSDKCall();
        if (g_Call_SkipProp[type] == INVALID_HANDLE)
            return SetError("Failed to prepare SkipProp call.");
    }

    return true;
}

bool Fakedelay_Start()
{
    if (!g_detours[DETOUR_SV_COMPUTECLIENTPACKS].Enable(Fakedelay_Detour_Pre_SV_ComputeClientPacks, Fakedelay_Detour_Post_SV_ComputeClientPacks))
        return false;

    if (!g_detours[DETOUR_SENDSNAPSHOT].Enable(Fakedelay_Detour_Pre_CGameClient__SendSnapshot, Fakedelay_Detour_Post_CGameClient__SendSnapshot))
        return false;

    return true;
}

void Fakedelay_Stop()
{
    g_detours[DETOUR_SV_COMPUTECLIENTPACKS].Disable(Fakedelay_Detour_Pre_SV_ComputeClientPacks, Fakedelay_Detour_Post_SV_ComputeClientPacks);

    g_detours[DETOUR_SENDSNAPSHOT].Disable(Fakedelay_Detour_Pre_CGameClient__SendSnapshot, Fakedelay_Detour_Post_CGameClient__SendSnapshot);
}

void Fakedelay_Hook_sv_parallel_sendsnapshot(ConVar convar, const char[] value_old, const char[] value_new)
{
    if (convar.BoolValue && g_settings[SETTING_FAKEDELAY].working)
    {
        if (g_settings[SETTING_FAKEDELAY].numusing > 0)
            Fakedelay_Stop();

        g_settings[SETTING_FAKEDELAY].working = false;
        g_settings[SETTING_FAKEDELAY].numusing = 0;

        bool setting_default = g_settings[SETTING_FAKEDELAY].GetDefault();
        for (int client = 1; client <= MAXPLAYERS; client++) {
            g_settings[SETTING_FAKEDELAY].active[client] = false;
            g_settings[SETTING_FAKEDELAY].values[client] = setting_default;
            g_settings[SETTING_FAKEDELAY].prefs[client] = setting_default;
        }
    }
    else {
        UnhookConVarChange(convar, Fakedelay_Hook_sv_parallel_sendsnapshot);
        g_settings[SETTING_FAKEDELAY].Init(-1, false);
    }
}

float g_fillpos[MAXPLAYERS+1][MAX_PROJECTILES][3];
float g_fillrot[MAXPLAYERS+1][MAX_PROJECTILES][3];
float g_fillvel[MAXPLAYERS+1][MAX_PROJECTILES][3];
float g_fillangvel[MAXPLAYERS+1][MAX_PROJECTILES][3];
MRESReturn Fakedelay_Detour_Pre_SV_ComputeClientPacks(DHookParam hParams)
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

            g_projs[client][i].message.pos.bit = -1;
            g_projs[client][i].message.rot.bit = -1;
            g_projs[client][i].message.vel.bit = -1;
            g_projs[client][i].message.angvel.bit = -1;

            SendProp prop;

            // Force vectors with SPROP_COORD_MP_INTEGRAL flag to always be 17*3 bits long to have enough space by using a |value| >= 2^COORD_INTEGER_BITS_MP
            prop = g_projs[client][i].message.pos.prop;
            if (prop && !UseRealCoordMP(prop)) {
                GetEntDataVector(entity, prop.offset, g_fillpos[client][i]);
                SetEntDataVector(entity, prop.offset, {2048.0, 2048.0, 2048.0}, true);
            }

            prop = g_projs[client][i].message.rot.prop;
            if (prop && !UseRealCoordMP(prop)) {
                GetEntDataVector(entity, prop.offset, g_fillrot[client][i]);
                SetEntDataVector(entity, prop.offset, {2048.0, 2048.0, 2048.0}, true);
            }

            prop = g_projs[client][i].message.vel.prop;
            if (prop && !UseRealCoordMP(prop)) {
                GetEntDataVector(entity, prop.offset, g_fillvel[client][i]);
                SetEntDataVector(entity, prop.offset, {2048.0, 2048.0, 2048.0}, true);
            }

            prop = g_projs[client][i].message.angvel.prop;
            if (prop && !UseRealCoordMP(prop)) {
                GetEntDataVector(entity, prop.offset, g_fillangvel[client][i]);
                SetEntDataVector(entity, prop.offset, {2048.0, 2048.0, 2048.0}, true);
            }
        }
    }

    return MRES_Ignored;
}

MRESReturn Fakedelay_Detour_Post_SV_ComputeClientPacks(DHookParam hParams)
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

            SendProp prop;

            prop = g_projs[client][i].message.pos.prop;
            if (prop && !UseRealCoordMP(prop))
                SetEntDataVector(entity, prop.offset, g_fillpos[client][i], false);

            prop = g_projs[client][i].message.rot.prop;
            if (prop && !UseRealCoordMP(prop))
                SetEntDataVector(entity, prop.offset, g_fillrot[client][i], false);

            prop = g_projs[client][i].message.vel.prop;
            if (prop && !UseRealCoordMP(prop))
                SetEntDataVector(entity, prop.offset, g_fillvel[client][i], false);

            prop = g_projs[client][i].message.angvel.prop;
            if (prop && !UseRealCoordMP(prop))
                SetEntDataVector(entity, prop.offset, g_fillangvel[client][i], false);

            if (!FindProjectileBitOffsets(client, i))
                continue;

            BitBuffer buffer = g_framesnapshotmanager.packeddata.Get(entity).data;

            SendPropInfo propinfo;

            propinfo = g_projs[client][i].message.pos;
            if (propinfo.bit != -1 && !UseRealCoordMP(propinfo.prop))
                buffer.WritePropVector(propinfo, propinfo.bit, g_fillpos[client][i]);

            propinfo = g_projs[client][i].message.rot;
            if (propinfo.bit != -1 && !UseRealCoordMP(propinfo.prop))
                buffer.WritePropVector(propinfo, propinfo.bit, g_fillrot[client][i]);

            propinfo = g_projs[client][i].message.vel;
            if (propinfo.bit != -1 && !UseRealCoordMP(propinfo.prop))
                buffer.WritePropVector(propinfo, propinfo.bit, g_fillvel[client][i]);

            propinfo = g_projs[client][i].message.angvel;
            if (propinfo.bit != -1 && !UseRealCoordMP(propinfo.prop))
                buffer.WritePropVector(propinfo, propinfo.bit, g_fillangvel[client][i]);
        }
    }

    return MRES_Ignored;
}

int g_datapos[MAX_PROJECTILES][3+1];
int g_datarot[MAX_PROJECTILES][3+1];
int g_datavel[MAX_PROJECTILES][3+1];
int g_dataangvel[MAX_PROJECTILES][3+1];
MRESReturn Fakedelay_Detour_Pre_CGameClient__SendSnapshot(Address pThis, DHookParam hParams)
{
    int client = LoadFromAddress(pThis + view_as<Address>(16), NumberType_Int32);
    if (!IsActivePlayer(client))
        return MRES_Ignored;

    if (g_sessions[client].fakedelay < 0)
        return MRES_Ignored;

    for (int i = 0; i < g_numprojs[client]; i++) {
        int entity = g_projs[client][i].Entity();
        if (entity == -1)
            continue;

        if (!g_projs[client][i].IsPredictable())
            continue;

        BitBuffer buffer = g_framesnapshotmanager.packeddata.Get(entity).data;

        SendPropInfo propinfo;

        propinfo = g_projs[client][i].message.pos;
        if (propinfo.bit != -1)
            for (int offset = 0; offset < 3+1; offset++)
                g_datapos[i][offset] = LoadFromAddress(view_as<Address>(buffer) + view_as<Address>((propinfo.bit / 32) * 4) + view_as<Address>(offset*4), NumberType_Int32);

        propinfo = g_projs[client][i].message.rot;
        if (propinfo.bit != -1)
            for (int offset = 0; offset < 3+1; offset++)
                g_datarot[i][offset] = LoadFromAddress(view_as<Address>(buffer) + view_as<Address>((propinfo.bit / 32) * 4) + view_as<Address>(offset*4), NumberType_Int32);

        propinfo = g_projs[client][i].message.vel;
        if (propinfo.bit != -1)
            for (int offset = 0; offset < 3+1; offset++)
                g_datavel[i][offset] = LoadFromAddress(view_as<Address>(buffer) + view_as<Address>((propinfo.bit / 32) * 4) + view_as<Address>(offset*4), NumberType_Int32);

        propinfo = g_projs[client][i].message.angvel;
        if (propinfo.bit != -1)
            for (int offset = 0; offset < 3+1; offset++)
                g_dataangvel[i][offset] = LoadFromAddress(view_as<Address>(buffer) + view_as<Address>((propinfo.bit / 32) * 4) + view_as<Address>(offset*4), NumberType_Int32);

        int frame = g_tickcount_frame + g_projs[client][i].clientdelay - RoundToCeil(g_sessions[client].fakedelay / 1000.0 / g_globals.interval_per_tick);

        ProjectileState state;
        state = g_projs[client][i].GetState(frame);

        propinfo = g_projs[client][i].message.pos;
        if (propinfo.bit != -1)
            buffer.WritePropVector(propinfo, propinfo.bit, state.pos);

        propinfo = g_projs[client][i].message.rot;
        if (propinfo.bit != -1)
            buffer.WritePropVector(propinfo, propinfo.bit, state.rot);

        propinfo = g_projs[client][i].message.vel;
        if (propinfo.bit != -1)
            buffer.WritePropVector(propinfo, propinfo.bit, state.vel);

        propinfo = g_projs[client][i].message.angvel;
        if (propinfo.bit != -1)
            buffer.WritePropVector(propinfo, propinfo.bit, state.angvel);
    }

    return MRES_Ignored;
}

MRESReturn Fakedelay_Detour_Post_CGameClient__SendSnapshot(Address pThis, DHookParam hParams)
{
    int client = LoadFromAddress(pThis + view_as<Address>(16), NumberType_Int32);
    if (!IsActivePlayer(client))
        return MRES_Ignored;

    if (g_sessions[client].fakedelay < 0)
        return MRES_Ignored;

    for (int i = 0; i < g_numprojs[client]; i++) {
        int entity = g_projs[client][i].Entity();
        if (entity == -1)
            continue;
        
        if (!g_projs[client][i].IsPredictable())
            continue;

        BitBuffer buffer = g_framesnapshotmanager.packeddata.Get(entity).data;

        SendPropInfo prop;

        prop = g_projs[client][i].message.pos;
        if (prop.bit != -1)
            for (int offset = 0; offset < 3+1; offset++)
                StoreToAddress(view_as<Address>(buffer) + view_as<Address>((prop.bit / 32) * 4) + view_as<Address>(offset*4), g_datapos[i][offset], NumberType_Int32, false);

        prop = g_projs[client][i].message.rot;
        if (prop.bit != -1)
            for (int offset = 0; offset < 3+1; offset++)
                StoreToAddress(view_as<Address>(buffer) + view_as<Address>((prop.bit / 32) * 4) + view_as<Address>(offset*4), g_datarot[i][offset], NumberType_Int32, false);

        prop = g_projs[client][i].message.vel;
        if (prop.bit != -1)
            for (int offset = 0; offset < 3+1; offset++)
                StoreToAddress(view_as<Address>(buffer) + view_as<Address>((prop.bit / 32) * 4) + view_as<Address>(offset*4), g_datavel[i][offset], NumberType_Int32, false);

        prop = g_projs[client][i].message.angvel;
        if (prop.bit != -1)
            for (int offset = 0; offset < 3+1; offset++)
                StoreToAddress(view_as<Address>(buffer) + view_as<Address>((prop.bit / 32) * 4) + view_as<Address>(offset*4), g_dataangvel[i][offset], NumberType_Int32, false);
    }

    return MRES_Ignored;
}