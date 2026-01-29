/* Generated from JOVIAL program: FLIGHT'CONTROL */
/* JOVIAL J73 to C transpiler */

#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#define max_waypoints 100
#define max_altitude 50000
#define min_airspeed 150
#define gravity 32.174
typedef float coord_type;
typedef int16_t altitude_type;
typedef float speed_type;
typedef uint16_t heading_type;
typedef enum {
    ground,
    takeoff,
    cruise,
    combat,
    landing,
    emergency
} flight_mode_t;
flight_mode_t flight_mode;
typedef enum {
    nominal,
    caution,
    warning,
    failure
} system_status_t;
system_status_t system_status;
static coord_type current_lat;
static coord_type current_lon;
static altitude_type current_alt;
static heading_type current_heading;
static speed_type current_airspeed;
coord_type target_lat;
coord_type target_lon;
altitude_type target_alt;
typedef struct {
    coord_type lat;
    coord_type lon;
    altitude_type alt;
    speed_type speed;
    bool active;
} waypoints_entry_t;

waypoints_entry_t waypoints[max_waypoints];

uint8_t current_waypoint = 1;
uint8_t total_waypoints = 0;
typedef struct {
    float reading;
    bool valid;
    uint32_t timestamp;
} sensors_entry_t;

sensors_entry_t sensors[8];

void calculate_distance(coord_type lat1, coord_type lon1, coord_type lat2, coord_type lon2, speed_type* distance) {
    coord_type dlat;
    coord_type dlon;
    dlat = (lat2 - lat1);
    dlon = (lon2 - lon1);
    *distance = sqrt((pow(dlat, 2) + pow(dlon, 2)));
}

void update_navigation(void) {
    float dist_to_waypoint;
    float waypoint_threshold = 0.5;
    calculate_distance(current_lat, current_lon, waypoints[(current_waypoint - 1)].lat, waypoints[(current_waypoint - 1)].lon, &dist_to_waypoint);
    if ((dist_to_waypoint < waypoint_threshold)) {
        if ((current_waypoint < total_waypoints)) {
            current_waypoint = (current_waypoint + 1);
        }
    }
}

void check_altitude_limits(void) {
    if ((current_alt > max_altitude)) {
        system_status = warning;
    }
    if ((current_alt < 0)) {
        system_status = failure;
        flight_mode = emergency;
    }
}

void check_airspeed_limits(void) {
    if ((current_airspeed < min_airspeed)) {
        if ((flight_mode != ground)) {
            system_status = caution;
        }
    }
}

void main_loop(void) {
    update_navigation();
    check_altitude_limits();
    check_airspeed_limits();
    switch (flight_mode) {
    case ground:
        ;
        break;
    case takeoff:
        if ((current_airspeed > min_airspeed)) {
            if ((current_alt > 100)) {
                flight_mode = cruise;
            }
        }
        break;
    case cruise:
        ;
        break;
    case combat:
        ;
        break;
    case landing:
        ;
        break;
    case emergency:
        ;
        break;
    }
}


int main(void) {
    flight_mode = ground;
    system_status = nominal;
    current_waypoint = 1;
    main_loop();
    return 0;
}
