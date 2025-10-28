import * as React from "react";

export function DecorativeBackground() {
  return (
    <div className="flex gap-2.5 items-start w-full max-md:max-w-full">
      <div className="flex flex-col flex-1 shrink items-center pb-24 w-full basis-0 min-w-60 max-md:max-w-full">
        <div className="flex self-stretch w-full bg-purple-50 fill-purple-50 min-h-[245px] max-md:max-w-full" />
        <div className="flex items-start max-w-full w-[786px]">
          <div className="flex flex-wrap gap-10 min-w-60 w-[786px] max-md:max-w-full">
            <div className="flex shrink-0 max-w-full bg-purple-300 h-[75px] w-[262px]" />
            <div className="flex shrink-0 max-w-full bg-purple-300 h-[75px] w-[262px]" />
          </div>
          <div className="flex shrink-0 bg-white h-[75px] min-w-60 w-[262px]" />
        </div>
        <BlogGrid />
      </div>
    </div>
  );
}

// Import BlogGrid here to avoid circular dependency
import { BlogGrid } from "./BlogGrid";
